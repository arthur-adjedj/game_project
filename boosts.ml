open Graphics
open Tick_manager
open Assets
open Missiles
open Plane
open Wind_boost
open Score

(*Boosts are small glowing balls that makes the plane go back up when hit*)


let boost_image () = Array.map make_image boost_sprites

let boost_dimensions = (Array.length boost_sprites.(0).(0),Array.length boost_sprites.(0))

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
  List.iter (draw_boost ((!tick/ticks_per_boost_update) mod 3)) !boosts


let has_hit_boost () = 
  let rec aux bl ml = match bl,ml with
    |[],_ -> []
    |l1,[] -> l1
    |h1::r1,(p,_)::_ -> 
      if are_in_collision h1 boost_dimensions p (missile_width,missile_height) then begin
        let (a,b) = h1 in
        add_score 1000;
        set_color black;
        fill_rect a b (snd boost_dimensions) (snd boost_dimensions);
        incr is_up;
        add_wind ();
        (aux r1 ml)
      end else h1::(aux r1 ml)
  in boosts := aux !boosts !missiles
    

let reset_boosts () =
  init_boosts ();
  draw_boosts ()