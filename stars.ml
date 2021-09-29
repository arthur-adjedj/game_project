open Graphics
open Assets

let stars () = Array.map make_image stars_arrs

let stars_pos = ref [||]

let n_variations = Array.length stars_arrs

let init_stars () = 
  Random.self_init ();
  let n = Random.int 50 in 
  stars_pos := Array.make n ((0,0),0);
  for i=0 to n-1 do
    !stars_pos.(i) <- ((Random.int 800,Random.int 800),Random.int n_variations)
  done
  
let draw_star i = 
  let ((a,b),c) = !stars_pos.(i) in
  draw_image (stars ()).(c) a b


let draw_stars () =
  let n = Array.length !stars_pos in
  for i=0 to n-1 do 
    draw_star i
  done

let reset_stars () =
  init_stars ();
  draw_stars ()
