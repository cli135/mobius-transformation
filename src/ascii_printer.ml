open Core

let print_ascii_image (image : float list) (image_width : int) : unit =
  (* input is a grayscale list *)
  let image_width = image_width in
  (* change density here if needed *)
  let pixel_ascii_map = "                                                                                             ````````````````````````````````````````````````````````````````````````````````````````...................................................................................------------------------------------------------------------------------------'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::_______________________________________________________________,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^================================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<++++++++++++++++++++++++++++!!!!!!!!!!!!!!!!!!!!!!!rrrrrrrrrrrrrrrrrrccccccccccccc********///z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@" in
  let max_val : float = List.fold ~f:(fun acc elt -> if Float.(elt > acc) then elt else acc)
                          ~init:(1.)
                          image
  in
  (* the below is rendering the images *)
  (List.iteri
    image
    ~f:(fun idx elt ->
      let image_width = image_width in
      let j = idx mod image_width in
      let value = elt in
      let ascii_val_idx = (int_of_float (Float.( * ) (Float.(/) (value) (max_val)) (float_of_int ((String.length pixel_ascii_map) - 1)))) in
      let ascii_val = String.get pixel_ascii_map ascii_val_idx in
      (* width multiplier is 2 *)
      print_string (String.make 1 ascii_val);
      print_string (String.make 1 ascii_val);
      if j = image_width - 1 then print_endline "" else print_string ""
    ))
