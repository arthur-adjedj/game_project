open Graphics
open Plane
open Buildings
open Bombs

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
  fill_rect 0 0 800 800;
  update_plane ();
  update_bombs ();
  update_buildings ();
  draw_bombs ();
  draw_plane ();
  draw_buildings ();
  if has_crashed () then game_over := true




let () = open_graph "800x800";
  auto_synchronize false;

  (*randomizes the size of the buildings once*)
  init_buildings ();

  (*uwu*)
  uwu;

  while true (*not !game_over*) do 
    if button_down () then add_bomb ();
      synchronize ();
      next_tick ();
  done ;

  print_string "game over :";
  set_color red;
  draw_image (make_image (Array.make_matrix 80 80 red)) 300 300;

  while true do
    ()
  done
