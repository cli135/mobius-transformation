
let tf ?(y=1) ~x z = x+2*y+z
(*  

open Core

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
  end

module MakeNVec (_ : sig val n : int end) : Vec = struct 
  type t =  

end

*)