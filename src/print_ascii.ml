(* open Image *)
(* open ImageLib_unix *)
open Rasterizer
open Math
(* open Ascii_printer *)

(* function to create a png image from the grayscale array given from rasterizer *)
let create_png_image (image : float list) (image_width : int) (output_filename : string): unit =
  let png_img = Image.create_rgb image_width image_width in
  let () = List.iteri (
    fun (idx : int) elt ->
      Image.write_grey png_img (idx mod image_width) (idx / image_width) (int_of_float (255. *. elt))
  ) image
  in
  ImageLib_unix.writefile output_filename png_img 
  
(* function to load any png image into ascii art, like sampling that image *)

(* main method *)
let () =
  let image_width = 500 in
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Planar ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  (* let () = print_ascii_image img image_width in *)
  let () = create_png_image img image_width "planar.png" in
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Sphere ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  (* let () = print_ascii_image img image_width in *)
  let () = create_png_image img image_width "sphere.png" in
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Orthogonal ~alpha:(Degree.of_float 20.) ~beta:(Degree.of_float 25.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  (* let () = print_ascii_image img image_width in *)
  let () = create_png_image img image_width "orthogonal.png" in
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Planar ~alpha:(Degree.of_float 0.) ~beta:(Degree.of_float 0.)
        ~center:(Vec3.of_list [ 1.; 1.; 1. ]) in
  (* let () = print_ascii_image img image_width in *)
  let () = create_png_image img image_width "test_planar_center_1_1_1.png" in
  let img =
    getImage ~img_w:image_width ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
        ~grid_size:2 Planar ~alpha:(Degree.of_float 0.) ~beta:(Degree.of_float 0.)
        ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
  (* let () = print_ascii_image img image_width in *)
  let () = create_png_image img image_width "test_planar_center_0_0_1.png" in
  ()