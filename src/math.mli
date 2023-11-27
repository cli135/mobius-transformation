(*
   This file is for defining basic math functions that are used in other files
   It defines a N-valued vector, auxiliary functions and several maps that are handy in Moebius transformation
*)

(* basic types *)

module type Vec = (* N dimensional vector*)
sig
  type t

  val n : int
  val nth : t -> int -> float
  val set : t -> int -> float -> unit
  val to_list : t -> float list
  val of_list : float list -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : float -> t -> t (* scaler multiplycation *)
  val sqr_length : t -> float
  val length : t -> float
  val unit : t -> t
  val dot : t -> t -> float
  val print : t -> unit
end

module MakeNVec (_ : sig
  val n : int
end) : Vec

module Vec2 : Vec
module Vec3 : Vec

(* helper functions *)
module Degree : sig
  type t
  val of_float : float -> t
  val to_radian : t -> float
end

val gt : float -> float -> bool
val cross : Vec3.t -> Vec3.t -> Vec3.t
val vec3ofvec2 : Vec3.t -> Vec2.t

(* geometric maps *)

(* compute the projection between x^2+y^2+(z-z_c)^2=1 sphere to the z=0 plane, requires z_c > 0
   input and output are all global coordinates
*)
val sProj : Vec3.t -> z_c:float -> Vec2.t

(* inverse transform of previous function *)
val invSProj : Vec2.t -> z_c:float -> Vec3.t

(* this is a S2->R^2 function so given a point on sphere(locally) we know where to sample from *)
val mapSphereSample : Vec3.t -> alpha:float -> beta:float -> Vec2.t

(* this is a R^2 -> R^2 function such that given (X,Y), we should sample from (X',Y') which is the location given by inverse Moebius transformation *)
val mapMoebius : Vec2.t -> alpha:float -> beta:float -> center:Vec3.t -> Vec2.t
