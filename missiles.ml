open Graphics
open Buildings
open Plane
open Score
open Assets


let missile_height = Array.length missile_arr    
let missile_width = Array.length missile_arr.(0)

let missile_bitmap = missile_arr
let drop_speed = 7

let missile_poss = ref []

let add_missile () = missile_poss:= ((iof (fst !pos) + 45,iof (snd !pos)-20),(iof (fst !pos),iof (snd !pos)-20))::!missile_poss

let update_missiles () = 
  let rec new_pos_list = function
    |[] -> []
    |((a,b),_)::r -> if b > 0 then
                    ((a,b - drop_speed),(a,b))::(new_pos_list r)
                    else (
                      set_color black;
                      fill_rect a 0 missile_width missile_height;
                      new_pos_list r)
in missile_poss := new_pos_list !missile_poss




let draw_missile ((a,b),(c,d)) = 
  let missile = make_image missile_bitmap in
  set_color black;
  fill_rect c d missile_width missile_height;
  draw_image missile a b

let draw_missiles () = List.iter draw_missile !missile_poss

let has_hit i ((a,b),_) = 
  let h = b_heights.(i) in
  h <> 0 &&
  are_in_collision (a,b) (missile_width,missile_height) (i*(block_width+40),0) (block_width,(h*block_height))


let update_buildings () = 
  let current = ref !missile_poss in
  
  let rec aux i = function
    |[] -> []
    |((a,b),(c,d))::r -> if has_hit i ((a,b),(c,d)) then 
            (
             set_color black;
             fill_rect a b missile_width missile_height;
             fill_rect c d missile_width missile_height;
             destroy_building i;
             add_score 500; aux i r)
             else ((a,b),(c,d))::(aux i r)
  in 

  for i=0 to n_of_buildings - 1 do 
    current := aux i !current
  done;

  missile_poss := !current
