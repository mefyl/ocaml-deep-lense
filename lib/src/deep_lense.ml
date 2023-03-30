open Ppxlib
module Fields = Map.Make (String)

type 'ast node =
  | Value of 'ast
  | Values of 'ast node Fields.t

exception
  Error of {
    loc : location;
    message : string;
  }

let pp_fields =
  let open Format in
  pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt '.') pp_print_string

let node_of_fields fields =
  let f fields (id, value) =
    let rec path acc = function
      | Lident name -> String.uncapitalize_ascii name :: acc
      | Ldot (id, name) -> path (String.uncapitalize_ascii name :: acc) id
      | Lapply _ ->
        raise
          (Error { loc = id.loc; message = "unexpected functor application" })
    in
    let path = path [] id.txt in
    let rec insert ctx fields = function
      | [] -> failwith "impossible empty path"
      | [ field ] ->
        let update = function
          | None -> Some (Value value)
          | Some _ ->
            raise
              (Error
                 {
                   loc = id.loc;
                   message =
                     Format.asprintf "duplicate field %a" pp_fields
                       (List.rev (field :: ctx));
                 })
        in
        Fields.update field update fields
      | head :: tail ->
        let ctx = head :: ctx in
        let update = function
          | None -> Some (Values (insert ctx Fields.empty tail))
          | Some (Values values) -> Some (Values (insert ctx values tail))
          | Some (Value _) ->
            raise
              (Error
                 {
                   loc = id.loc;
                   message =
                     Format.asprintf "duplicate field %a" pp_fields
                       (List.rev ctx);
                 })
        in
        Fields.update head update fields
    in
    insert [] fields path
  in
  List.fold_left f Fields.empty fields

let expression =
  let expand ~ctxt fields value =
    try
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let module Ast = Ast_builder.Make (struct
        let loc = loc
      end) in
      let rec to_fields ctx fields =
        let fold name value acc =
          let id = { loc; txt = Lident name } in
          match value with
          | Value v -> (id, v) :: acc
          | Values v ->
            let ctx = Ast.pexp_field ctx id in
            ({ loc; txt = Lident name }, to_record ctx v) :: acc
        in
        Fields.fold fold fields []
      and to_record ctx v = Ast.pexp_record (to_fields ctx v) (Some ctx) in
      [%expr
        (fun x -> x)
          [%e to_record value @@ node_of_fields fields]
        [@ocaml.warning "-useless-record-with"]]
    with Error { loc; message } ->
      [%expr [%ocaml.error [%e Ast_builder.Default.estring ~loc message]]]
  in
  Extension.V3.declare "deep_lense" Expression
    Ast_pattern.(single_expr_payload @@ pexp_record __ (some __))
    expand

let pattern =
  let expand ~ctxt fields _closed =
    try
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      let module Ast = Ast_builder.Make (struct
        let loc = loc
      end) in
      let rec to_fields node =
        let fold name value acc =
          let id = { loc; txt = Lident name } in
          match value with
          | Value v -> (id, v) :: acc
          | Values v -> ({ loc; txt = Lident name }, to_record v) :: acc
        in
        Fields.fold fold node []
      and to_record v = Ast.ppat_record (to_fields v) Open in
      [%pat? [%p to_record @@ node_of_fields fields]]
    with Error { loc; message } ->
      [%pat? [%ocaml.error [%e Ast_builder.Default.estring ~loc message]]]
  in
  Extension.V3.declare "deep_lense" Pattern
    Ast_pattern.(ppat (ppat_record __ __) none)
    expand

let () =
  Driver.register_transformation
    ~rules:
      [
        Ppxlib.Context_free.Rule.extension expression;
        Ppxlib.Context_free.Rule.extension pattern;
      ]
    "deep-lense"
