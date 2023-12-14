(* open Core *)
(* open Bimage *)

(* This is grid data structure (like a 2D array)
   except instead of being made of arrays, it is made of lists *)
(* At some point it may be advisable to make these OCaml arrays,
if the size is not changing and we are making mutations common *)
(* module List_grid :
   sig
      (* val width : int
      val height : int *)
      type 'a t = 'a list list
      val make_list_grid : int -> int -> 'a t
      val to_list : ...
      val of_list : ...
   end *)

(* This represents a single RGB value of floats,
   which will later be used with List_grid to make a data structure
   RGB.t List_grid.t *)
(* module RGB :
   sig
      type t =
         { r : float
         ; g : float
         ; b : float }
   end *)

(* This will turn a Bimage.Image.t into a 2D list of rgb 3-tuples,
   which is easier to work on with Core.List in OCaml *)
(* val list_list_of_image : Bimage.Image.t -> RGB.t List_grid.t *)

(* This is the main function is that is made available to be called
   so that images represented in the Bimage format can be printed to the terminal *)
(* val print_ascii_image : RGB.t Grid.t -> unit *)
val print_ascii_image : float list -> int -> unit
