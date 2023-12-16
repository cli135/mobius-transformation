(* This is grid data structure (like a 2D array)
   except instead of being made of arrays, it is made of lists *)
module Gray_image :
   sig
      type t = float list list
      val to_float_list : t -> float list
      val of_float_list : float list -> int -> t
   end

(* This is the main function is that is made available to be called
   so that images represented in the float list format can be printed to the terminal *)
val print_ascii_image : Gray_image.t -> int -> unit

(* This generates a PNG image from a Gray_image (internal representation to PNG) *)
val create_png_image : Gray_image.t -> int -> string -> unit
