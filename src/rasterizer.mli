(*

(* 
    This part is responsible for doing all the rastering work that are gonna be used in the ascii printer
    The user are supposed to mostly use the ["alpha", "beta", "center"] parameters
    The rest are included optinally with default arguments to customize the viewport if necessary
*)
open Core
open Math 
open Ascii_printer

val getImage : (mode:int -> alpha:float -> beta:float -> center:Vec3 -> ?camera_offset:int -> ?view_direction:Vec3 -> ?img_w:int ->
     -> ?viewsize:int -> ?bd:int -> ?half_edge:int -> ?line_w:float -> ?grid_size:int -> RGB.t Grid.t)

*)

(* Syntax Note to myself
    val tf: ?y: int -> x:int -> int -> int
    let tf ?(y=1) ~x z = x+2*y+z
*)