open Graphics
open Buildings
open Plane


let bomb_bitmap = Array.make_matrix 20 50 red
let drop_speed = 40

let bomb_poss = ref []

let add_bomb () = bomb_poss:=!pos::!bomb_poss

let update_bombs () = 
  let rec new_pos_list = function
    |[] -> []
    |(a,b)::r -> if b > 0 then
                    (a,b - drop_speed)::(new_pos_list r)
                    else new_pos_list r
in bomb_poss := new_pos_list !bomb_poss




let draw_bomb (a,b) = 
  let bomb = make_image bomb_bitmap in
  draw_image bomb a b

let draw_bombs () = List.iter draw_bomb !bomb_poss

let has_hit i (a,b) = 
  let h = b_heights.(i) in
  are_in_collision (a,b) (20,50) (i*(block_width+40),0) ((h*block_height),block_width)


let update_buildings () = 
  let current = ref !bomb_poss in
  
  let rec aux i = function
    |[] -> []
    |h::r -> if has_hit i h then (destroy_building i; aux i r)
             else h::(aux i r)
  in 

  for i=0 to n_of_buildings - 1 do 
    current := aux i !current
  done;

  bomb_poss := !current




