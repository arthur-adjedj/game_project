open Plane

let drop_speed = 10

let bomb_poss = ref []

let new_bomb () = bomb_poss:=!pos::!bomb_poss

let update_positions () = 
  let rec new_pos_list = function
    |[] -> []
    |(a,b)::r -> (a,b - drop_speed)::(new_pos_list r)
in bomb_poss := new_pos_list !bomb_poss


let draw_bomb () = ()

