(* This part is responsible for doing all the rastering work that are gonna be used in the ascii printer  *)
open Math

(*
     The user are only supposed to use the last function,
    the rest are to be used in test but are not expected to be called by the users
*)

(* sample if a point is on the grid lines *)
(* parameter list: p,    grid_size,width,half_edge_length *)
val sample_grid : Vec2.t -> int -> float -> int -> int

(* Given pixel coordinate, return color for planar view
   parameter list: i,j,grid_size,alphe,beta,center,image_w, viewsize, half_edge_length,width *)
val sample_plane :
  (* i *)
  int ->
  (* j *)
  int ->
  (* grid_size *)
  int ->
  (* alpha *)
  float ->
  (* beta *)
  float ->
  (* center *)
  Vec3.t ->
  (* image_w *)
  int ->
  (* viewsize *)
  int ->
  (* half_edge_length *)
  int ->
  (* width *)
  float ->
  float

(* Given pixel coordinate, return color for spherical view *)
val sample_sphere :
  (* i *)
  int ->
  (* j *)
  int ->
  (* grid_size *)
  int ->
  (* directions *)
  Vec3.t * Vec3.t * Vec3.t ->
  (* alpha *)
  float ->
  (* beta *)
  float ->
  (* image_w *)
  int ->
  (* half_edge_length *)
  int ->
  (* width *)
  float ->
  float

(* Given pixel coordinate, return the orthogonal projection view*)
val sample_orthogonal :
  (* i *)
  int ->
  (* j *)
  int ->
  (* grid_size *)
  int ->
  (* cameraoffset *)
  float ->
  (* directions *)
  Vec3.t * Vec3.t * Vec3.t ->
  (* alpha *)
  float ->
  (* beta *)
  float ->
  (* center *)
  Vec3.t ->
  (* image_w *)
  int ->
  (* viewsize *)
  int ->
  (* half_edge_length *)
  int ->
  (* plane_bd *)
  int ->
  (* width *)
  float ->
  float

(*
       The user are supposed to mostly use the ["alpha", "beta", "center"] parameters
       The rest are included optinally with default arguments to customize the viewport if necessary
       output is temperarily set to be bool, it will become RGB.t List_grid.t when we reach that point
*)

type render_mode = Planar | Sphere | Orthogonal

val getImage :
  ?camera_offset:float ->
  ?view_direction:Vec3.t ->
  ?img_w:int ->
  ?view_size:int ->
  ?bd:int ->
  ?half_edge:int ->
  ?line_w:float ->
  ?grid_size:int ->
  render_mode -> (* to get rid of warning 16 [unerasable-optional-argument] *)
  alpha:float -> (* both alpha/beta are in degree*)
  beta:float ->
  center:Vec3.t ->
  float list
