open Graphics
open Plane
open Buildings
open Bombs
open Score

let game_over = ref false

type int = color

let (mod) x y = 
  if x mod y <0 then 
    (x mod y) + y
else x mod y

let wait milli =
  ignore (Unix.select [] [] [] (milli/.1000.))

let print_tuple (a,b) = 
  print_string "(";
  print_int a;
  print_string ",";
  print_int b;
  print_string ")"


let background () = make_image (Array.make_matrix 800 800 white)


let next_tick () = 
  set_color black;
  fill_rect 0 0 800 800;
  update_plane ();
  update_bombs ();
  update_buildings ();
  draw_bombs ();
  draw_plane ();
  draw_buildings ();
  draw_score ();
  if has_crashed () then game_over := true


exception Game_Over


let () = open_graph "800x800";
  auto_synchronize false;

  (*randomizes the size of the buildings once*)
  init_buildings ();

  (*uwu*)
  uwu;

  while not !game_over do 
    let t = Sys.time () in
    if button_down () then add_bomb ();
      synchronize ();
      next_tick ();
    wait (max 0. (300. -. (Sys.time () -. t) ))
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
