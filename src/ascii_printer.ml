(*

(* Intentionally left empty until your implementation *)
open Core
(* 

(* Beginning draft of function *)

*)
let print_ascii_image (image : float list) : unit =
  (* input is a grayscale list *)
  (* let grid = make_list_grid 200 200 in *)
  let image_width = 200 in
  (* let image_height = 200 in *)
  (* let width_of_terminal = 120 in
  let aspect_ratio = 1 / (image_width / image_height) in *)
  (* let height_of_terminal = width_of_terminal / aspect_ratio in *)
  (* let pixel_ascii_map = " `.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@" in *)
  let pixel_ascii_map = "                                                                                             ````````````````````````````````````````````````````````````````````````````````````````...................................................................................------------------------------------------------------------------------------'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::_______________________________________________________________,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^================================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<++++++++++++++++++++++++++++!!!!!!!!!!!!!!!!!!!!!!!rrrrrrrrrrrrrrrrrrccccccccccccc********///z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@" in
  (* change density here if needed *)
  (* let width_multiplier : int = 2 in *)
  let max_val : float = List.fold ~f:(fun acc elt -> if Float.(elt > acc) then elt else acc)
                          ~init:(1.)
                          image
  in
  (* let traverse (idx : int) =    
  in *)
  (* the below is based on 200x200 *)
  (List.iteri
    image
    ~f:(fun idx elt ->
      let image_width = image_width in
      (* this should be a square image so image_width and image_height
         ought to be the same *)
      (* let image_height = List.length image / image_width in
      let i = idx / image_width in *)
      let j = idx mod image_width in
      (* let sampled_i = Float.( * ) (Float.(/) (float_of_int i) (float_of_int width_of_terminal)) (float_of_int image_width) in
      let sampled_j = Float.( * ) (Float.(/) (float_of_int j) (float_of_int height_of_terminal)) (float_of_int image_height) in
      (* similar to nearest point sampling, just rounding down or truncating *)
      let nearest_i : int = int_of_float sampled_i in
      let nearest_j : int = int_of_float sampled_j in *)
      let value = elt in
      let ascii_val_idx = (int_of_float (Float.( * ) (Float.(/) (value) (max_val)) (float_of_int ((String.length pixel_ascii_map) - 1)))) in
      let ascii_val = String.get pixel_ascii_map ascii_val_idx in
      (* width multiplier is 2 *)
      print_string (String.make 1 ascii_val);
      print_string (String.make 1 ascii_val);
      if j = image_width - 1 then print_endline "" else print_string ""
    ))
  
*)