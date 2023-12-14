(* This is grid data structure (like a 2D array)
   except instead of being made of arrays, it is made of lists *)
module Gray_image :
   sig
      (* val width : int *)
      (* val height : int *)
      type t = float list list
      (* val make_list_grid : int -> int -> t *)
      val to_float_list : t -> float list
      val of_float_list : float list -> int -> t
   end

(* This will turn a Bimage.Image.t into a 2D list of rgb 3-tuples,
   which is easier to work on with Core.List in OCaml *)
(* val list_list_of_image : Bimage.Image.t -> RGB.t List_grid.t *)

(* This is the main function is that is made available to be called
   so that images represented in the float list format can be printed to the terminal *)
val print_ascii_image : Gray_image.t -> int -> unit
val create_png_image : Gray_image.t -> int -> string -> unit
