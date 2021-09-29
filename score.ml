open Graphics

(*basic score system*)
let score = ref 0

let draw_score () =
  set_color black;
  fill_rect 10 700 200 20;
  set_color white;
  moveto 10 700;
  draw_string ("Score : " ^ string_of_int !score)

let add_score n = 
  score := !score + n