open Graphics
open Plane
open Buildings

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
  draw_image (background ()) 0 0;
  update_plane ();
  draw_buildings ();
  draw_plane ()



let () = open_graph "800x800";
  init_buildings ();

  uwu;

  while true do 
    wait 1000.;
    next_tick () 
  done




