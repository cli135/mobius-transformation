(* keyframe animation *)
open Math

type keyframe_params = { alpha : Degree.t; beta : Degree.t; center : Vec3.t }

val linear_interpolate : keyframe_params -> keyframe_params -> float -> keyframe_params

val generate_keyframes : keyframe_params -> keyframe_params -> int -> float -> (keyframe_params -> keyframe_params -> float -> keyframe_params) -> keyframe_params list

val get_cool_animation : int -> float -> keyframe_params list