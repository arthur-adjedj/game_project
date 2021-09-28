open Graphics
open Buildings
open Assets

exception Break
 
let plane_height = Array.length plane_matrix
let plane_width  = Array.length plane_matrix.(0)


(*whether this func workswith the bot_left origin system of Graphics needs to be verified*)
(*PRIORITY*)
let are_in_collision (x1,y1) (w1,h1) (x2,y2) (w2,h2) = 
    not (x1 > x2 + w2 ||
     x2 > x1 + w1 ||
     y1 > y2 + h2 ||
     y2 > y1 + h1)

let iof = int_of_float 

(*plane has to be a function, otherwise it will get initialised before the graph is open, returning an error*)
let plane () = make_image plane_matrix 
    
let pos = ref (-50.,700.) 

let last = ref !pos

let vel = ref (2.,-0.25) 

let draw_plane () = 
    set_color black;
    fill_rect (iof (fst !last)) (iof (snd !last)) 110 37;
    draw_image (plane ()) (iof (fst !pos)) (iof (snd !pos)) 


let update_plane () = 
    last := !pos;
    pos := (mod_float ((fst !pos) +. (fst  !vel))  800.,mod_float ((snd !pos) +. (snd !vel)) 800.);
    vel := (fst !vel +. 0.001, snd !vel -. 0.001) 


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

