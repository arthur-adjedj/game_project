open Graphics
open Assets
open Plane
open Tick_manager

(*WARNING*)
(*When a booster is hit, and the plane goes up, 
some wind sprite is supposed to appear underneath it,
this file is supposed to manage just that, but it doesn't work yet*)

let wind () = make_image wind_boost

let wind_dims = (Array.length wind_boost.(0),Array.length wind_boost)

let winds = ref []


let add_wind () = 
  winds := ((iof (fst !pos),iof (snd !pos)),ticks_of_wind) :: !winds
  

let update_winds () = 
  let rec aux l = match l with
    |[] -> []
    |((a,b),t)::r -> if t=0 then (
                      set_color black;
                      fill_rect a b (fst wind_dims) (snd wind_dims);
                      aux r)
      else (
        if t=ticks_of_wind then draw_image (wind ()) a b;
        ((a,b),t-1)::(aux r)
      )
        
    in winds := aux !winds