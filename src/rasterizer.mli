(* This part is responsible for doing all the rastering work that are gonna be used in the ascii printer  *)
open Math
(* open Ascii_printer *)

(*
       The user are supposed to mostly use the ["alpha", "beta", "center"] parameters
       The rest are included optinally with default arguments to customize the viewport if necessary
       output is temperarily set to be bool, it will become RGB.t List_grid.t when we reach that point

   val getImage : (mode:int -> alpha:float -> beta:float -> center:Vec3 -> ?camera_offset:int -> ?view_direction:Vec3 -> ?img_w:int ->
        -> ?viewsize:int -> ?bd:int -> ?half_edge:int -> ?line_w:float -> ?grid_size:int -> bool )
*)

(* the rest are to be used in test but are not expected to be called by the users *)

(* sample if a point is on the grid lines *)
val sample_grid : Vec2.t -> int -> float -> int -> int

(* Given pixel coordinate, return color for planar view
   parameter list: i,j,grid_size,alphe,beta,center,image_w, viewsize, half_edge_length,width *)
val sample_plane :
  int -> (* i *)
  int -> (* j *)
  int -> (* grid_size *)
  float -> (* alpha *)
  float -> (* beta *)
  Vec3.t ->  (* center *)
  int ->  (* image_w *)
  int -> (* viewsize *)
  int -> (* half_edge_length *)
  float -> (* width *)
  int

(* Given pixel coordinate, return color for spherical view *)
val sample_sphere :
  int -> (* i *)
  int -> (* j *)
  int -> (* grid_size *)
  Vec3.t * Vec3.t * Vec3.t -> (* directions *)
  float -> (* alpha *)
  float -> (* beta *)
  int ->  (* image_w *)
  int ->  (* half_edge_length *)
  float -> (* width *)
  float

(* Given pixel coordinate, return the orthogonal projection view*)
val sample_orthogonal :
  int -> (* i *)
  int -> (* j *)
  int -> (* grid_size *)
  float -> (* cameraoffset *)
  Vec3.t * Vec3.t * Vec3.t -> (* directions *)
  float -> (* alpha *)
  float -> (* beta *)
  Vec3.t -> (* center *)
  int -> (* image_w *)
  int -> (* plane_bd *)
  int -> (* half_edge_length *)
  float -> (* width *)
  float
