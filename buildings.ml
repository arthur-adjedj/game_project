open Graphics
open Assets


let b_vars () = Array.map make_image b_arrs
let n_vars = Array.length b_arrs

let grey = rgb 128 128 128 
let block_height = 50 
let block_width = 60
let building_shift = 40

let n_of_buildings = 800 / (block_width + building_shift)


let rumbles () = make_image rumbles_arr


let random_height () = 
  Random.self_init ();
  Random.int 8



let b_heights = Array.make n_of_buildings 0 

let init_buildings () =
  for i=0 to n_of_buildings - 1 do 
    b_heights.(i) <- random_height ()
  done

let draw_building k = 
  let h = b_heights.(k) in
  Random.self_init ();
  for i = 0 to h-1 do
    let r = Random.int n_vars in
    draw_image (b_vars ()).(r) (k*(block_width+building_shift)) (i*block_height)
  done



let draw_buildings () =
  for i = 0 to n_of_buildings-1 do 
  draw_building i
  done

let orange = rgb 128 64 0

let destroy_building k =
  let h = b_heights.(k)-1 in
  set_color black;
  fill_rect (k*(block_width+building_shift)) (h*block_height) block_width (2*block_height);
  draw_image (rumbles ()) (k*(block_width+building_shift)) (h*block_height);
  b_heights.(k) <- max 0 h


let uwu = ()
