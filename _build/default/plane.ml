open Graphics

let iof = int_of_float 


(*plane has to be a function, otherwise it will get initialised before the graph is open, returning an error*)
let plane () = make_image (Array.make_matrix 30 50 yellow) 
    
let pos = ref (50,700) 

let vel = ref (50.,-10.) 

let draw_plane () = 
    draw_image (plane ()) (fst !pos) (snd !pos) 


let update_plane () = 
    pos := (((fst !pos) + iof (fst  !vel)) mod 750,((snd !pos) + iof (snd !vel)) mod 800);
    vel := (fst !vel +. 1., snd !vel) 

