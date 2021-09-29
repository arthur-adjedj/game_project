open Graphics
open Tick_manager
open Plane
open Buildings
open Missiles
open Score
open Boosts
open Stars
open Wind_boost

let game_over = ref false

type int = color

let (mod) x y = 
  if x mod y <0 then 
    (x mod y) + y
else x mod y

let wait s =
  ignore (Unix.select [] [] [] (s))

let print_tuple (a,b) = 
  print_string "(";
  print_int a;
  print_string ",";
  print_int b;
  print_string ")"

let is_game_won () = Array.for_all (fun x -> x = 0) b_heights


(*if all buildings are destroyed, the game is reset,
keeping the old score*)
let reset () =
  set_color black;
  fill_rect 0 0 800 800;
  reset_buildings ();
  reset_missiles ();
  reset_plane ();
  reset_boosts ();
  reset_stars ();
  wait 1.

(*updates what needs to be updated on the screen*)
let next_tick () = 
  if is_game_won () then reset ();
  incr tick;
  missile_check ();
  set_color black;
  has_hit_boost ();
  update_winds ();
  update_plane ();
  update_missiles ();
  update_buildings ();
  draw_missiles ();
  draw_plane ();
  draw_score ();
  draw_boosts ();
  if has_crashed () then game_over := true 

  
(*main function*)
let () = open_graph "800x800";
  auto_synchronize false; (*this guarantees that things get entirely drawn before being rendered on the screen*)

  (*sets background*)
  set_color black;
  fill_rect 0 0 800 800;
  

  (*randomizes the size of the buildings once*)
  init_buildings ();
  (*initiales boosts and stars*)
  init_boosts ();
  init_stars ();
  wait 0.1;
  draw_stars ();
  draw_buildings ();

  (*uwu*)
  uwu;

  while not !game_over do 
    let t = Sys.time () in
      synchronize ();
      next_tick ();
      (*/!\ the speed of the game is framerate dependent, so a limit must be kept on how fast it can go*)
    wait (max 0. ((1./.tickrate) -. (Sys.time () -. t) )) 
  done ;

  (*game over screen*)
  if !game_over then begin
    set_color red;
    fill_rect 0 0 800 800;
    moveto 350 400;
    set_text_size 5;
    set_color black;
    draw_string "GAME OVER";
    moveto 310 380;
    set_text_size 3;
    draw_string ("final score : "^ string_of_int !score);
    synchronize ()
  end;

  (*this last part ensures the window doesn't close after the game is over*)
 while true do 
  ()
 done



