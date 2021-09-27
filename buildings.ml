open Graphics




let grey = rgb 128 128 128 
let block_height = 50 
let block_width = 60

let n_of_buildings = 800 / (block_width + 40)

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
  let building = make_image 
    (Array.make_matrix (h*block_height) block_width grey) in 
  draw_image building (k*(block_width+40)) 0 



let draw_buildings () =
  for i = 0 to n_of_buildings-1 do 
  draw_building i
  done

let destroy_building k =
  b_heights.(k) <-  max 0 (b_heights.(k) -1)

let is_game_won () = Array.for_all (fun x -> x = 0) b_heights

let uwu = ()
