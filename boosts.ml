open Graphics
open Tick_manager
open Assets

let boost_image () = Array.map make_image boost_sprites

let boosts = ref []

let add_boost () =
  Random.self_init ();
  let pos = (25 + Random.int 750,Random.int 500) in 
  boosts := pos::!boosts
  

let init_boosts () = 
  Random.self_init ();
  for _=0 to Random.int 5 do 
    add_boost ()
  done

let draw_boost state pos =
  draw_image (boost_image ()).(state) (fst pos) (snd pos)

let draw_boosts () = 
  List.iter (draw_boost ((!tick/tick_per_missile) mod 3)) !boosts








