# Deep lense for OCaml records

A ppx rewriter providing easy overrides of deeply nested record
fields.

```ocaml
type point = { x : int; y : int }
and player = { name : string; position : point }
and game = { player_a : player; player_b: player }

let game = {
  player_a = { name = "Alice"; position = { x = 0; y = 0 } };
  player_b = { name = "Bob"; position = { x = 10; y = 10 } };
}

(* Overriding game.player_a.position.x *)

(* Plain OCaml *)
let _ = {
  game with player_a = {
    game.player_a with position = {
      game.player_a.position with x = 1
    }
  }
}

(* With deep-lense *)
let _ = [%deep_lense { game with Player_a.Position.x = 1 }]
```

Nested field are capitalized since `{ game with player_a.position.x =
1 }` is not syntactically valid. Qualifying fields with module names
and thus not possible, but doing so is unnecessary since the base
record type is known.
