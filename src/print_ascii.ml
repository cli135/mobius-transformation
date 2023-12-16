open Core
open Rasterizer
open Math
open Ascii_printer


(* main method *)
let () =
  let image_width = 80 in
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

let rec looping () =
  Out_channel.output_string stdout "Welcome to the print_ascii.exe program!\n";
  Out_channel.output_string stdout "PNGs are automatically generated in the root directory. \n";
  Out_channel.output_string stdout "To convert a PNG to ASCII, enter a filename: \n";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> looping ()
  | Some "exit" -> ()
  | Some input_filename ->
    (* let all_files_in_cwd = Sys_unix.ls_dir "." in *)
    (* match List.find all_files_in_cwd ~f:(fun x -> String.(x = input_filename)) with *)
    begin
    match Sys_unix.file_exists input_filename with
    | `Yes -> 
      let image = ImageLib_unix.openfile input_filename in
      let image_width = image.width in
      let image_height = image.height in
      let float_list = 
        List.init (image_width * image_height) ~f:(fun x -> x)
        |> List.map ~f:(fun (idx : int) ->
          Image.read_grey image (idx mod image_width) (idx / image_width) (fun x -> x)
        )
        |> List.map ~f:(fun x -> float_of_int x)
      in
      let () = print_ascii_image (Gray_image.of_float_list float_list image_width) image_width
      in 
      looping ()
    | _ ->
      Out_channel.output_string stdout
      ("Could not find file with filename " ^ input_filename ^ ", please try again\n");
      looping ()
    end

let () = looping ()