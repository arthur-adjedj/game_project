open Graphics
open Tick_manager
open Buildings
open Plane
open Score
open Assets

(*last tick where the plane shot a missile, used to block shooting frequency*)
let last_tick_shot = ref 0

let missile_height = Array.length missile_arr    
let missile_width = Array.length missile_arr.(0)

let missile_bitmap = missile_arr


let drop_speed = 10

(*each missiles are handled as a tuple of positions,the first one representing the current pos,
the second the last (used to erase the old sprite of each missile)*)
let missiles = ref []

let add_missile () = missiles:= ((iof (fst !pos) + 45,iof (snd !pos)-20),(iof (fst !pos),iof (snd !pos)-20))::!missiles

let update_missiles () = 
  let rec new_pos_list = function
    |[] -> []
    |((a,b),_)::r -> if b > 0  then
                    ((a,b - drop_speed),(a,b))::(new_pos_list r)
                    else ( (*if b<=missile_height, the missile is out of the screen and needs to be removed and erased*)
                      set_color black;
                      fill_rect a 0 missile_width missile_height;
                      new_pos_list r)
in missiles := new_pos_list !missiles (*updates the n° and positions of missiles*)




let draw_missile ((a,b),(c,d)) = 
  let missile = make_image missile_bitmap in
  set_color black;
  fill_rect c d missile_width missile_height; (*erases past missile*)
  draw_image missile a b

let draw_missiles () = List.iter draw_missile !missiles

(*checks collision between a missile and the building i*)
let has_hit i ((a,b),_) = 
  let h = b_heights.(i) in
  h <> 0 &&
  are_in_collision (a,b) (missile_width,missile_height) (i*(block_width+40),0) (block_width,(h*block_height))


(*checks collisions of all misiles with all buidings and destroys them accordingly*)
let update_buildings () = 
  destroy_rumbles (); 
  let current = ref !missiles in
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

  missiles := !current

let missile_check () = 
  if (!tick - !last_tick_shot) >= tick_per_missile && button_down () then (add_missile ();last_tick_shot := !tick)


let reset_missiles () = 
  missiles := []