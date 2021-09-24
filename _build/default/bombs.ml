open Plane

let bomb_poss = Stack.create ()

let new_bomb () = Stack.push !pos bomb_poss 