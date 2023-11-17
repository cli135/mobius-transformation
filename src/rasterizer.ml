open Math
open Core

(* sample from a regular grid pattern *)
let sample_grid v2 grid_size width half_edge_length =
  let x, y = (Vec2.nth v2 0, Vec2.nth v2 1)
  and grid_size_, half_edge_length_ =
    (float_of_int grid_size, float_of_int half_edge_length)
  in
  if
    (* if outside target area *)
    gt (Float.abs x) (half_edge_length_ +. (width *. 0.5))
    || gt (Float.abs y) (half_edge_length_ +. (width *. 0.5))
  then 0
  else
    let xm, ym =
      ( x *. grid_size_ /. half_edge_length_,
        y *. grid_size_ /. half_edge_length_ )
    and width_scaled = width *. 0.5 *. grid_size_ /. half_edge_length_ in
    if
      (* if on grid lines *)
      gt width_scaled (Float.abs @@ (Float.round xm -. xm))
      || gt width_scaled (Float.abs @@ (Float.round ym -. ym))
    then 1
    else 0

(* the image of plane *)
(*
   Note: the direction of image indexing is as follows
     |-------> increase of j
     |   (0,0)  (0,1)
     |   (1,0)  (1,1)
    \|/
     increase of i
*)

let sample_plane i j grid_size alpha beta center image_w viewsize
    half_edge_length width =
  let viewsize_f = float_of_int viewsize and image_w_f = float_of_int image_w in
  let p =
    Vec2.of_list
      [
        -.viewsize_f +. (2.0 *. float_of_int j /. image_w_f *. viewsize_f);
        viewsize_f -. (2.0 *. float_of_int i /. image_w_f *. viewsize_f);
      ]
  in
  let p_on_plane = mapMoebius p ~alpha ~beta ~center in
  sample_grid p_on_plane grid_size width half_edge_length

(* ratio of a pixel in the image, helper function*)
let ratio i image_w_f = ((2. *. float_of_int i) -. image_w_f) /. image_w_f

(* project a point to sphere *)
let pointOnSphere p_start forward_dir =
  let d1 = -.Vec3.dot p_start forward_dir in
  let d2 =
    Vec3.sqr_length p_start -. (d1 *. d1) |> Float.max 1e-6 |> Float.sqrt
    (* the max here in computation is to avoid floating point error, sometimes d1^2 > 1 when it should not, especially when i,j = width/2,width/2 *)
  in
  if gt d2 1. then None
  else
    let l = Float.sqrt (1. -. (d2 *. d2)) in
    Some (Vec3.( * ) (d1 -. l) forward_dir |> Vec3.( + ) p_start |> Vec3.unit)

(* Given pixel coordinate, return color for spherical view *)
let sample_sphere i j grid_size directions alpha beta image_w half_edge_length
    width =
  let forward_dir, right_dir, up_dir = directions
  and image_w_f = float_of_int image_w in
  let p_start =
    Vec3.( * ) 2. forward_dir
    |> Fn.flip Vec3.( - ) (Vec3.( * ) (ratio i image_w_f) up_dir)
    |> Vec3.( + ) (Vec3.( * ) (ratio j image_w_f) right_dir)
  in
  let p_on_sphere_opt = pointOnSphere p_start forward_dir in
  match p_on_sphere_opt with
  | None -> 0.
  | Some p_on_sphere ->
      let p_on_plane = mapSphereSample p_on_sphere ~alpha ~beta in
      let c = sample_grid p_on_plane grid_size width half_edge_length in
      if c = 0 then 0.2 else 1.

(*
   The vector computation at this point is really getting uglier...
   and also the needed transformation in between int <-> float all the time.
   Maybe there are better ways to do scientific computing in Ocaml than the way I am doing it rn
*)

(* intersection of a ray and the z=0 plane *)
let planeIntersection p_start forward_dir =
  let t = Vec3.nth p_start 2 /. Vec3.nth forward_dir 2 in
  Vec3.( + ) p_start (Vec3.( * ) t forward_dir)

(* Given pixel coordinate, return the orthogonal projection view*)
let sample_orthogonal i j grid_size cameraoffset directions alpha beta center
    image_w plane_bd half_edge_length width =
  let forward_dir, right_dir, up_dir = directions
  and image_w_f = float_of_int image_w in
  let p_start =
    Vec3.of_list [ 0.; 0.; cameraoffset ]
    |> Fn.flip Vec3.( - ) (Vec3.( * ) 1.5 forward_dir)
    |> Fn.flip Vec3.( - ) (Vec3.( * ) (ratio i image_w_f) up_dir)
    |> Vec3.( + ) (Vec3.( * ) (ratio j image_w_f) right_dir)
  in
  let p_start_loc = Vec3.( - ) p_start center in
  let p_on_sphere_opt = pointOnSphere p_start_loc forward_dir in
  match p_on_sphere_opt with
  | None ->
      (* sample from plane *)
      let p_on_plane = planeIntersection p_start forward_dir in
      if
        (* check the plane boundary because we don't want the plane to fill up the whole image*)
        Vec3.nth p_on_plane 0 |> Float.abs |> Fn.flip gt (float_of_int plane_bd)
        || Vec3.nth p_on_plane 1 |> Float.abs
           |> Fn.flip gt (float_of_int plane_bd)
      then 0.
      else
        let c =
          sample_grid
            (mapSphereSample p_on_plane ~alpha ~beta)
            grid_size width half_edge_length
        in
        if c = 0 then 0.4 else 1.
  | Some p_on_sphere ->
      (* sample from sphere*)
      let p_on_plane = mapSphereSample p_on_sphere ~alpha ~beta in
      let c = sample_grid p_on_plane grid_size width half_edge_length in
      if c = 0 then 0.2 else 1.
