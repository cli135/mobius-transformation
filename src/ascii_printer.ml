open Core

(* This is grid data structure (like a 2D array)
   except instead of being made of arrays, it is made of lists *)
   module Gray_image = struct
      type t = float list list
      let to_float_list (grid : t) : float list = List.concat grid
      let of_float_list (float_list : float list) (width : int) : t =
        if (List.length float_list) mod width <> 0 then
          failwith "The length of the float list is not a multiple of the width, i.e. not rectangular image"
        else
          List.chunks_of float_list ~length:(width)
end

let get_pixel_density_map (png: bool) : string =
  if png then
    " `.-':_,^=;><+!rc*/z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@"
  else "                                                                                             ````````````````````````````````````````````````````````````````````````````````````````...................................................................................------------------------------------------------------------------------------'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::_______________________________________________________________,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^================================================;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<++++++++++++++++++++++++++++!!!!!!!!!!!!!!!!!!!!!!!rrrrrrrrrrrrrrrrrrccccccccccccc********///z?sLTv)J7(|Fi{C}fI31tlu[neoZ5Yxjya]2ESwqkP6h9d4VpOGbUAKXHm8RD#$Bg0MNWQ%&@"

let print_ascii_image (gray_image : Gray_image.t) (image_width : int) ~(png : bool): unit =
  let image = Gray_image.to_float_list gray_image in
  (* input is a grayscale list *)
  let image_width = image_width in
  (* change density here if needed *)
  let pixel_ascii_map = get_pixel_density_map png in
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

(* function to create a png image from the grayscale array given from rasterizer *)
let create_png_image (gray_image : Gray_image.t) (image_width : int) (output_filename : string): unit =
  let image = Gray_image.to_float_list gray_image in 
  let png_img = Image.create_rgb image_width image_width in
  let () = List.iteri ~f:(
    fun (idx : int) elt ->
      Image.write_grey png_img (idx mod image_width) (idx / image_width) (int_of_float (255. *. elt))
  ) image
  in
  ImageLib_unix.writefile output_filename png_img 
  
(* function to load any png image into ascii art, like sampling that image *)
