open Graphics
open Tick_manager
open Plane
open Buildings
open Missiles
open Score
open Boosts
open Stars

let game_over = ref false
let game_won = ref false

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


let is_game_won () = game_won :=Array.for_all (fun x -> x = 0) b_heights

let next_tick () = 
  incr tick;
  missile_check ();
  set_color black;
  (*fill_rect 0 0 800 800;*)
  update_plane ();
  update_missiles ();
  update_buildings ();
  draw_stars ();
  draw_missiles ();
  draw_plane ();
  draw_score ();
  draw_boosts ();
  is_game_won ();
  if has_crashed () then game_over := true 


exception Game_Over



let () = open_graph "800x800";
  auto_synchronize false;

  (*sets background*)
  set_color black;
  fill_rect 0 0 800 800;
  

  (*randomizes the size of the buildings once*)
  init_buildings ();
  init_boosts ();
  init_stars ();
  draw_buildings ();

  (*uwu*)
  uwu;

  while not (!game_over || !game_won) do 
    let t = Sys.time () in
      synchronize ();
      next_tick ();
    wait (max 0. ((1./.tickrate) -. (Sys.time () -. t) ))
  done ;

  
  if !game_over then begin
    set_color red;
    fill_rect 0 0 800 800;
    moveto 350 400;
    set_text_size 5;
    set_color black;
    draw_string "GAME OVER";
    synchronize ()
  end
  else begin
    set_color green;
    fill_rect 0 0 800 800;
    moveto 350 400;
    set_text_size 5;
    set_color black;
    draw_string "GAME WON !";
    moveto 310 380;
    set_text_size 3;
    draw_string ("final score : "^ string_of_int !score);
    synchronize ()
  end;

 while true do 
  plot 0 0
 done



