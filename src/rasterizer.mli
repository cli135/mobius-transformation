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
  ?camera_offset:float -> (* camera height, only applies in orthogonal view*)
  ?view_direction:Vec3.t -> (* camera forward direction *)
  ?img_w:int -> (* image width *)
  ?view_size:int -> (* the planar view size, e.g., in planar view we sees the [-view_size, view_size] x [-view_size, view_size] area *)
  ?bd:int -> (* the orthogonal view boudnary. This is to prevent the planer image occupies the whole orthogonal view *)
  ?half_edge:int -> (* the length of grid. The grid sit in the [-half_edge, half_edge] x [-half_edge, half_edge] area *)
  ?line_w:float -> (* width of grid lines *)
  ?grid_size:int -> (* number of lines. We will have grid_size grids and grid_size+1 vertical/horizontal corresponding lines *)
  render_mode -> (* to get rid of warning 16 [unerasable-optional-argument] *)
  alpha:float -> (* rotation along z axis in degree*)
  beta:float -> (* rotation along y axis in degree *)
  center:Vec3.t -> (* center of the sphere *)
  float list
