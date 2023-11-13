
(*  
  This file is for defining basic math functions that are used in other files
  It defines a N-valued vector, auxiliary functions and several maps that are handy in Moebius transformation
*)

(*
open Core

(* basic types *)

module type Vec = (* N dimensional vector*)
  sig
    type t
    val n: int (* dimension *)
    val ( + ): t -> t -> t
    val ( - ): t -> t -> t
    val ( * ): float -> t -> t (* scaler multiplycation *)
    val length: t -> float
    val sqr_length: t -> float
    val create: float list -> t
    val unit: t -> t
    val dot: t -> t -> t
    val to_list: t -> float list
    val nth: int -> t -> float
  end

module MakeNVec (_ : sig val n : int end) : Vec
(*
module Vec2 : Vec
module Vec3 : Vec

val cross: Vec3 -> Vec3 -> Vec3

(* geometric maps *)

val stereoProj: Vec3 -> float -> Vec2
val istereoProj: Vec2 -> float -> Vec3

val mapMoebius: Vec2 -> alpha:float -> beta:float -> center:Vec3 -> Vec2

(* helper function *)
val rad: float -> float 

*)
*)