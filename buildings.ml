open Graphics
open Assets
open Tick_manager

exception Building_image_failure
let b_vars () = 
  try Array.map make_image b_arrs
with Graphic_failure(_) -> raise Building_image_failure
let n_vars = Array.length b_arrs

let grey = rgb 128 128 128 
let block_height = 50 
let block_width = 60
let building_shift = 40

let n_of_buildings = 800 / (block_width + building_shift)

(*rumbles will be drawn for one frame whenever a building's floor gets destroyed*)
let rumbles () = make_image rumbles_arr


let are_in_rumbles = ref []

let random_height () = 
  Random.self_init ();
  Random.int 8



let b_heights = Array.make n_of_buildings 0

(*sets random heights for each building*)
let init_buildings () =
  for i=0 to n_of_buildings - 1 do 
    b_heights.(i) <- random_height ()
  done

(*draws a single building*)
let draw_building k = 
  let h = b_heights.(k) in
  Random.self_init ();
  for i = 0 to h-1 do
    let r = Random.int n_vars in
    draw_image (b_vars ()).(r) (k*(block_width+building_shift)) (i*block_height)
  done


(*draws all buildings, should only be called once when the game is initialized, slows the game down **drastically** *)
let draw_buildings () =
  for i = 0 to n_of_buildings-1 do 
  draw_building i
  done

(*erases the highest floor of building k*)
let destroy_building k =
  let h = b_heights.(k)-1 in
  set_color black;
  fill_rect (k*(block_width+building_shift)) (h*block_height) block_width (2*block_height);
  draw_image (rumbles ()) (k*(block_width+building_shift)) (h*block_height);
  are_in_rumbles := (k,ticks_of_rumble)::!are_in_rumbles;
  b_heights.(k) <- max 0 h

let destroy_rumble k =
  let h = b_heights.(k)-1 in
  set_color black;
  fill_rect (k*(block_width+building_shift)) ((h+1)*block_height) block_width (block_height)

let destroy_rumbles () = 
  let rec aux l = match l with
    |[] -> []
    |(k,t)::r -> if t=0 then (destroy_rumble k; aux r)
                 else (k,t-1)::(aux r)
  in are_in_rumbles := aux !are_in_rumbles 

  
(*uwu*)
let uwu = ()
