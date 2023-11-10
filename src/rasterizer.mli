open Core
open Math 


val getImage : (mode:int -> camera_offset:int -> view_direction:Vec3 -> img_w:int ->
    alpha:float -> beta:float -> center:Vec3 -> viewsize:int -> bd:int 
    -> half_edge:int -> line_w:float -> grid_size:int)