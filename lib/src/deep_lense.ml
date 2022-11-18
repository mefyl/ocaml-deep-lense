open Ppxlib
module Fields = Map.Make (String)

type node = Value of Ast.expression | Values of node Fields.t

exception Error of { loc : location; message : string }

let pp_fields =
  let open Format in
  pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt '.') pp_print_string

let expand ~ctxt fields value =
  try
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let module Ast = Ast_builder.Make (struct
      let loc = loc
    end) in
    let fields =
      let f fields (id, value) =
        let rec path acc = function
          | Lident name -> String.uncapitalize_ascii name :: acc
          | Ldot (id, name) -> path (String.uncapitalize_ascii name :: acc) id
          | Lapply _ ->
              raise
                (Error
                   { loc = id.loc; message = "unexpected functor application" })
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
    in
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
        [%e to_record value fields] [@ocaml.warning "-useless-record-with"]]
  with Error { loc; message } ->
    [%expr [%ocaml.error [%e Ast_builder.Default.estring ~loc message]]]

let my_extension =
  Extension.V3.declare "deep_lense" Expression
    Ast_pattern.(single_expr_payload @@ pexp_record __ (some __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "deep-lense"
