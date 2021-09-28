open Graphics
open Plane
open Buildings
open Missiles
open Score

let tickrate = 60.

let tick_per_missile = 10

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


let background () = make_image (Array.make_matrix 800 800 white)

let tick = ref 0

let next_tick () = 
  if !tick mod tick_per_missile = 0 && button_down () then add_missile ();
  set_color black;
  (*fill_rect 0 0 800 800;*)
  update_plane ();
  update_missiles ();
  update_buildings ();
  draw_missiles ();
  draw_plane ();
  draw_score ();
  if has_crashed () then game_over := true;
  incr tick


exception Game_Over


let () = open_graph "800x800";
  auto_synchronize false;

  (*sets background*)
  set_color black;
  fill_rect 0 0 800 800;

  (*randomizes the size of the buildings once*)
  init_buildings ();
  draw_buildings ();

  (*uwu*)
  uwu;

  while not !game_over do 
    let t = Sys.time () in
      synchronize ();
      next_tick ();
    wait (max 0. ((1./.tickrate) -. (Sys.time () -. t) ))
  done ;

  

  set_color red;
  fill_rect 0 0 800 800;
  moveto 350 400;
  set_text_size 5;
  set_color black;
  draw_string "GAME OVER";
  synchronize ();

 while true do 
  plot 0 0
 done



