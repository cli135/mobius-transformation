(*
Should we remove underline underscore in Ascii_printer filename and library name
*)
(*

open Core
open Bimage

module List_grid :
   sig
      type 'a t = 'a list list
   end

module RGB :
   sig
      type t =
         { r : float
         ; g : float
         ; b : float }
   end

(* This will turn a Bimage.Image.t into a 2D list of rgb 3-tuples,
   which is easier to work on with Core.List in OCaml *)
val list_list_of_image : Bimage.Image.t -> RGB.t Grid.t

(* This is the main function is that is made available to be called
   so that images represented in the Bimage format can be printed to the terminal *)
val print_ascii_image : Bimage.Image.t -> unit

*)
