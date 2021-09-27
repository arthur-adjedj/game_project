open Graphics
open Buildings


exception Break

(*whether this func workswith the bot_left origin system of Graphics needs to be verified*)
(*PRIORITY*)
let are_in_collision (x1,y1) (w1,h1) (x2,y2) (w2,h2) = 
    not (x1 > x2 + w2 ||
     x2 > x1 + w1 ||
     y1 > y2 + h2 ||
     y2 > y1 + h1)

let iof = int_of_float 

(*plane has to be a function, otherwise it will get initialised before the graph is open, returning an error*)
let plane () = make_image (Array.make_matrix 30 50 yellow) 
    
let pos = ref (-50,700) 

let vel = ref (50.,-10.) 

let draw_plane () = 
    draw_image (plane ()) (fst !pos) (snd !pos) 


let update_plane () = 
    pos := (((fst !pos) + iof (fst  !vel)) mod 800,((snd !pos) + iof (snd !vel)) mod 800);
    vel := (fst !vel , snd !vel) 


let has_crashed () = 
    let res = ref false in
    try
        for k = 0 to n_of_buildings - 1 do 
            let h = b_heights.(k) in
            if  are_in_collision !pos (30,50) (k*(block_width+40),0) (block_width,(h*block_height)) then 
                (res:=true;
                raise Break)
        done;
        !res
    with Break -> !res

