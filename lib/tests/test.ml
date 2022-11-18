type point = { x : int; y : int }
and player = { name : string; position : point } [@@deriving eq, show]

let player = Alcotest.testable pp_player equal_player
let alice = { name = "Alice"; position = { x = 0; y = 0 } }

let single () =
  Alcotest.check player "lense" alice
    [%deep_lense { alice with Position.x = 0 }];
  Alcotest.check player "lense"
    { alice with position = { alice.position with x = 1 } }
    [%deep_lense { alice with Position.x = 1 }]

let multiple () =
  Alcotest.check player "lense" alice
    [%deep_lense { alice with Position.x = 0; Position.y = 0 }];
  Alcotest.check player "lense"
    { alice with position = { x = 1; y = 1 } }
    [%deep_lense { alice with Position.x = 1; Position.y = 1 }];
  Alcotest.check player "lense"
    { name = "Bob"; position = { x = 1; y = 1 } }
    [%deep_lense { alice with position = { x = 1; y = 1 }; name = "Bob" }]

(* let _ = [%deep_lense { alice with Position.x = 0; Position.x = 0 }] *)

(* let _ = [%deep_lense { alice with position = { x = 0; y = 0 }; Position.x = 0 }] *)

let () =
  let open Alcotest in
  run "deep-lense"
    [
      ( "basic",
        [
          test_case "single" `Quick single; test_case "multiple" `Quick multiple;
        ] );
    ]
