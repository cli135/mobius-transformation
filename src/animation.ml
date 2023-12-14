(* keyframe animation *)

open Math
open Core

type keyframe_params = { alpha : Degree.t; beta : Degree.t; center : Vec3.t }

let linear_interpolate (k1 : keyframe_params) (k2 : keyframe_params) (t : float)
    : keyframe_params =
  let alpha =
    Degree.(t * (k2.alpha - k1.alpha) + k1.alpha)
  and beta =
    Degree.(t * (k2.beta - k1.beta) + k1.beta)
  and center =
    Vec3.(t * (k2.center - k1.center) + k1.center)
  in
  { alpha; beta; center }

let generate_keyframes (k1 : keyframe_params) (k2 : keyframe_params)
    (frame_rate : int) (duration : float)
    (interpolation_method :
      keyframe_params -> keyframe_params -> float -> keyframe_params) :
    keyframe_params list =
  let total_frames : int = int_of_float (float_of_int frame_rate *. duration) in
  let map2keyframe (frame: int) : keyframe_params =
    interpolation_method k1 k2 (float_of_int frame /. float_of_int total_frames) in
  List.range 0 (total_frames + 1) |> List.map ~f:map2keyframe

(* the following part is for the hardcoded animation *)

(* this arc is a special one so I am just going to hard code it *)
let keyframe_list_arc ?(r = 2.) frame_rate duration change_angle =
  let total_frames = int_of_float (float_of_int frame_rate *. duration) in
  let half_frames = total_frames / 2 in
  let get_location i =
    if i < half_frames then
      let theta =
        float_of_int i /. float_of_int half_frames /. 2. *. Float.pi
      in
      Vec3.of_list [ r *. Float.sin theta; r -. (r *. Float.cos theta); 1. ]
    else
      let theta =
        float_of_int (i - half_frames)
        /. float_of_int half_frames /. 2. *. Float.pi
      in
      Vec3.of_list [ r -. (r *. Float.sin theta); r *. Float.cos theta; 1. ]
  in
  let get_params i =
    if change_angle then
      {
        alpha =
          Degree.of_float
          @@ (180. /. float_of_int total_frames *. float_of_int i);
        beta =
          Degree.of_float
          @@ (360. /. float_of_int total_frames *. float_of_int i);
        center = get_location i;
      }
    else
      {
        alpha = Degree.of_float 0.;
        beta = Degree.of_float 0.;
        center = get_location i;
      }
  in
  List.map ~f:get_params (List.range 0 (total_frames + 1))

(* hard coded animation to play *)
let get_cool_animation current_frame_rate current_duration =
  List.concat
    [
      keyframe_list_arc current_frame_rate current_duration false;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1. ];
        }
        {
          alpha = Degree.of_float 180.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1. ];
        }
        current_frame_rate (current_duration *. 0.8) linear_interpolate;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1. ];
        }
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 4. ];
        }
        current_frame_rate (current_duration *. 0.4) linear_interpolate;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 4. ];
        }
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1.5 ];
        }
        current_frame_rate (current_duration *. 0.4) linear_interpolate;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1.5 ];
        }
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 360.;
          center = Vec3.of_list [ 0.; 0.; 1.5 ];
        }
        current_frame_rate current_duration linear_interpolate;
      keyframe_list_arc current_frame_rate (current_duration *. 2.) true;
    ]
