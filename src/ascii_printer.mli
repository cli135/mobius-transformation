(*

open Core
open Bimage

(* This will turn a Bimage.Image.t into a 2D list of rgb 3-tuples,
   which is easier to work on with Core.List in OCaml *)
val list_list_of_image : Bimage.Image.t -> (float * float * float) list list

(* This is the main function is that is made available to be called
   so that images represented in the Bimage format can be printed to the terminal *)
val print_ascii_image : Bimage.Image.t -> unit

*)
