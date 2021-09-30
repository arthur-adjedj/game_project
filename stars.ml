open Graphics
open Assets

let stars () = Array.map make_image stars_arrs

let stars_pos = ref []

let n_variations = Array.length stars_arrs

let init_stars () = 
  Random.self_init ();
  let n = Random.int 50 in 
  let rec aux i = 
    if i=0 then []
    else ((Random.int 800,Random.int 800),Random.int n_variations)::(aux (i-1))
  in stars_pos := aux n
  
let draw_star ((a,b),c)  = 
  draw_image (stars ()).(c) a b


let draw_stars () =
  List.iter draw_star !stars_pos

let reset_stars () =
  init_stars ();
  draw_stars ()
