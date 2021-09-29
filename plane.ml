open Graphics
open Buildings
open Assets
open Tick_manager


exception Break
 
let plane_height = Array.length plane_matrix
let plane_width  = Array.length plane_matrix.(0)

(*checks if plane should go up from a boost in the next frame*)
let is_up = ref 0

let is_up_frames = ref 0

(*rectangle collisions*)
let are_in_collision (x1,y1) (w1,h1) (x2,y2) (w2,h2) = 
    not (x1 > x2 + w2 ||
     x2 > x1 + w1 ||
     y1 > y2 + h2 ||
     y2 > y1 + h1)

let iof = int_of_float 

(*images have to be unit functions, otherwise they will get initialised before the graph is open, returning an error*)
let plane () = make_image (if !is_up_frames > 0 then(decr is_up_frames; plane_up_matrix )else plane_matrix )
    

let base_pos = (-50.,700.)
(*spawn position, gets updated every tick*)
let pos = ref base_pos

(*last position, used to erase the plane drawn during the last tick*)
let last = ref !pos

(*self explanatory*)
let vel = ref (2.,-0.25) 


let draw_plane () = 
    set_color black; 
    fill_rect (iof (fst !last)) (iof (snd !last)) 110 37; (*erases the last plane*)
    draw_image (plane ()) (iof (fst !pos)) (iof (snd !pos)) (*draws the new one*)


(*updates the position of the plane*)
let update_plane () = 
    last := !pos; 
    if !is_up > 0 then begin  (*if boost was taken, go up*)
        decr is_up;
        is_up_frames := up_frames;
        pos := (mod_float ((fst !pos) +. (fst  !vel))  800. ,mod_float ((snd !pos) +. 100.) 800.)
    end else begin  (*otherwise, go down*)
        pos := (-. 100. +. mod_float ((fst !pos) +. (fst  !vel) +. 100.)  900. ,mod_float ((snd !pos) +. (snd !vel)) 800.);
    end;
    vel := (fst !vel +. 0.001, snd !vel -. 0.001)  (*updates velocity (optional)*)


(*is used to check game over*)
let has_crashed () = 
    let res = ref false in
    try
        for k = 0 to n_of_buildings - 1 do 
            let h = b_heights.(k) in
            if  are_in_collision (iof (fst !pos),iof (snd !pos)) (plane_width,plane_height) (k*(block_width+40),0) (block_width,(h*block_height)) then 
                (res:=true;
                raise Break)
        done;
        !res
    with Break -> !res


let reset_plane () = 
    is_up := 0;
    pos := base_pos