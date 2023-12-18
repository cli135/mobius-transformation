open Math
open Core
open Ascii_printer

module Exposed_for_testing = struct
  (*
   This module is exposed for testing purpose
   It contains all the functions that are used in the main module
   but are not exposed to the user
*)

  (* sample from a regular grid pattern *)
  let sample_grid v2 grid_size width half_edge_length =
    let x, y = (Vec2.nth v2 0, Vec2.nth v2 1)
    and grid_size_, half_edge_length_ =
      (float_of_int grid_size, float_of_int half_edge_length)
    in
    if
      (* if outside target area *)
      Float.(abs x > half_edge_length_ +. (width *. 0.5))
      || Float.(abs y > half_edge_length_ +. (width *. 0.5))
    then 0
    else
      let xm, ym =
        ( x *. grid_size_ /. half_edge_length_,
          y *. grid_size_ /. half_edge_length_ )
      and width_scaled = width *. 0.5 *. grid_size_ /. half_edge_length_ in
      if
        (* if on grid lines *)
        Float.(width_scaled > abs @@ (round xm -. xm))
        || Float.(width_scaled > abs @@ (round ym -. ym))
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

  let sample_plane ~i ~j ~grid_size ~alpha ~beta ~center ~img_w ~view_size
      ~half_edge_length ~line_w =
    let viewsize_f = float_of_int view_size
    and image_w_f = float_of_int img_w in
    let p =
      Vec2.of_list
        [
          -.viewsize_f +. (2.0 *. j /. image_w_f *. viewsize_f);
          viewsize_f -. (2.0 *. i /. image_w_f *. viewsize_f);
        ]
    in
    let p_on_plane = mapMoebius p ~alpha ~beta ~center in
    sample_grid p_on_plane grid_size line_w half_edge_length |> float_of_int

  (* ratio of a pixel in the image, helper function*)
  let ratio i image_w_f = ((2. *. i) -. image_w_f) /. image_w_f

  (* project a point to sphere *)
  let pointOnSphere p_start forward_dir =
    let d1 = -.Vec3.dot p_start forward_dir in
    let d2 =
      Vec3.sqr_length p_start -. (d1 *. d1) |> Float.max 1e-6 |> Float.sqrt
      (* the max here in computation is to avoid floating point error, sometimes d1^2 > 1 when it should not, especially when i,j = width/2,width/2 *)
    in
    if Float.(d2 > 1.) then None
    else
      let l = Float.sqrt (1. -. (d2 *. d2)) in
      Some Vec3.(unit @@ (((d1 -. l) * forward_dir) + p_start))

  (* Given pixel coordinate, return color for spherical view *)
  let sample_sphere ~i ~j ~grid_size ~directions ~alpha ~beta ~img_w
      ~half_edge_length ~line_w =
    let forward_dir, right_dir, up_dir = directions
    and image_w_f = float_of_int img_w in
    let p_start =
      Vec3.(
        (2. * forward_dir)
        - (ratio i image_w_f * up_dir)
        + (ratio j image_w_f * right_dir))
    in
    let p_on_sphere_opt = pointOnSphere p_start forward_dir in
    match p_on_sphere_opt with
    | None -> 0.
    | Some p_on_sphere ->
        let p_on_plane = mapSphereSample p_on_sphere ~alpha ~beta in
        let c = sample_grid p_on_plane grid_size line_w half_edge_length in
        if c = 0 then 0.2 else 1.

  (*
   I changed most of the vector computation to be a readibility version but maybe I missed some..
*)

  (* intersection of a ray and the z=0 plane *)
  let planeIntersection p_start forward_dir =
    let t = -.Vec3.nth p_start 2 /. Vec3.nth forward_dir 2 in
    Vec3.((t * forward_dir) + p_start) |> vec3tovec2

  (* Given pixel coordinate, return the orthogonal projection view*)
  let sample_orthogonal ~i ~j ~grid_size ~camera_offset ~directions ~alpha ~beta
      ~center ~img_w ~view_size ~half_edge_length ~plane_bd ~line_w =
    let forward_dir, right_dir, up_dir = directions
    and image_w_f = float_of_int img_w
    and view_size_f = float_of_int view_size in
    let p_start =
      Vec3.(
        of_list [ 0.; 0.; camera_offset ]
        - (1.5 * forward_dir)
        - (ratio i image_w_f *. view_size_f * up_dir)
        + (ratio j image_w_f *. view_size_f * right_dir))
    in
    let p_start_loc = Vec3.(p_start - center) in
    let p_on_sphere_opt = pointOnSphere p_start_loc forward_dir in
    match p_on_sphere_opt with
    | None ->
        (* sample from plane *)
        let p_on_plane = planeIntersection p_start forward_dir in
        if
          (* check the plane boundary because we don't want the plane to fill up the whole image*)
          Float.(float_of_int plane_bd < abs (Vec2.nth p_on_plane 0))
          || Float.(float_of_int plane_bd < abs (Vec2.nth p_on_plane 1))
        then 0.
        else
          let c =
            sample_grid
              (mapMoebius p_on_plane ~alpha ~beta ~center)
              grid_size line_w half_edge_length
          in
          if c = 0 then 0.4 else 1.
    | Some p_on_sphere ->
        (* sample from sphere*)
        let p_on_plane = mapSphereSample p_on_sphere ~alpha ~beta in
        let c = sample_grid p_on_plane grid_size line_w half_edge_length in
        if c = 0 then 0.2 else 1.
end

open Exposed_for_testing

type render_mode = Planar | Sphere | Orthogonal

let cartesianProduct l1 l2 =
  List.concat @@ List.map l1 ~f:(fun x -> List.map l2 ~f:(fun y -> (x, y)))

(* based on view direction, compute forward, up, right directions *)
let compute_dir view_direction =
  let p_unit = Vec3.unit view_direction |> Vec3.( * ) (-1.) in
  let z_val = -.Vec3.nth p_unit 2 in
  let up_dir =
    if Float.(1e-10 > z_val) then Vec3.of_list [ 0.; 0.; 1. ]
    else Vec3.(of_list [ 0.; 0.; 1. /. z_val ] + p_unit) |> Vec3.unit
  in
  (p_unit, cross p_unit up_dir, up_dir)

(* get the int * int pixel, return a list of float * float as the supersampling points*)
let super_sampling_points c ij =
  let i_, j_ = ij in
  let i, j = (float_of_int i_, float_of_int j_) in
  match c with
  | 1 -> [ (i, j) ]
  | 4 ->
      (* this is hard coded bcs it's enough for this application*)
      [
        (i +. 0.25, j +. 0.25);
        (i +. 0.25, j -. 0.25);
        (i -. 0.25, j +. 0.25);
        (i -. 0.25, j -. 0.25);
      ]
  | _ -> failwith "undefined number of samples"

let getImage ?(camera_offset = 1.)
    ?(view_direction = Vec3.of_list [ 1.; -1.; 1. ]) ?(img_w = 100)
    ?(view_size = 8) ?(plane_bd = 4) ?(half_edge_length = 1) ?(line_w = 0.1)
    ?(grid_size = 4) ?(sampling_n = 1) mode ~alpha ~beta ~center =
  let indices =
    cartesianProduct (List.range 0 img_w) (List.range 0 img_w)
    |> List.map ~f:(super_sampling_points sampling_n)
  in
  let averaging l =
    List.fold l ~init:0. ~f:( +. ) /. float_of_int (List.length l)
  and directions = compute_dir view_direction in
  match mode with
  | Planar ->
      (* planar *)
      List.map indices ~f:(fun ij_list ->
          List.map ij_list ~f:(fun (i, j) ->
              sample_plane ~i ~j ~grid_size ~alpha:(Degree.to_radian alpha)
                ~beta:(Degree.to_radian beta) ~center ~img_w ~view_size
                ~half_edge_length ~line_w)
          |> averaging) |> Fn.flip Gray_image.of_float_list img_w
  | Sphere ->
      (* sample_sphere *)
      List.map indices ~f:(fun ij_list ->
          List.map ij_list ~f:(fun (i, j) ->
              sample_sphere ~i ~j ~grid_size ~directions
                ~alpha:(Degree.to_radian alpha) ~beta:(Degree.to_radian beta)
                ~img_w ~half_edge_length ~line_w)
          |> averaging) |> Fn.flip Gray_image.of_float_list img_w
  | Orthogonal ->
      (* sample orthogonal *)
      List.map indices ~f:(fun ij_list ->
          List.map ij_list ~f:(fun (i, j) ->
              sample_orthogonal ~i ~j ~grid_size ~camera_offset ~directions
                ~alpha:(Degree.to_radian alpha) ~beta:(Degree.to_radian beta)
                ~center ~img_w ~view_size ~half_edge_length ~plane_bd ~line_w)
          |> averaging) |> Fn.flip Gray_image.of_float_list img_w
