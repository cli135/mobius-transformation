(* Intentionally left empty until your implementation *)

open Core
open Math
open Rasterizer
open Ascii_printer

(* let () = *)
 let rec looping () =
  Out_channel.output_string stdout "enter something: \n";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> looping ()
  | Some "exit" -> ();
  | Some s ->
    Out_channel.output_string stdout 
         ("You just input: " ^ s ^"\n");
    Out_channel.flush stdout;
    looping () (* do something here, probably update internal values and redraw the ascii images *)
  (* in looping () *)

let display_static_image display_type alpha beta image_w : unit =
  match display_type with
  | "planar" -> 
    let img =
      getImage ~img_w:image_w ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
          ~grid_size:2 Planar ~alpha:(Degree.of_float alpha) ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
    let () = print_ascii_image img image_w in ()
  | "sphere" -> 
    let img =
      getImage ~img_w:image_w ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
          ~grid_size:2 Sphere ~alpha:(Degree.of_float alpha) ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
    let () = print_ascii_image img image_w in ()  
  | "orthogonal" -> 
    let img =
      getImage ~img_w:image_w ~view_size:4 ~plane_bd:4 ~half_edge_length:2 ~line_w:0.25
          ~grid_size:2 Orthogonal ~alpha:(Degree.of_float alpha) ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list [ 0.; 0.; 1. ]) in
    let () = print_ascii_image img image_w in ()  
  | _ ->
    let () = print_endline "display type command line argument should be one of 'planar', 'sphere', or 'orthogonal' "
    in ()




(* command line argument parsing *)
let command =
  Command.basic
    ~summary:
      "Make an n-gram distribution and allow certain queries to be run, like \
       sample from n-gram distribution to make a sentence"
    (* command line arguments are below *)
    (let%map_open.Command display_type =
        flag "--display-type" (optional_with_default ("") string) ~doc:"The type of display: sphere, planar, or orthogonal"
      and alpha_parameter =
        flag "--alpha" (optional_with_default (-1.) float) ~doc:"alpha parameter in moebius transformation"
      and beta_parameter =
        flag "--beta" (optional_with_default (-1.) float) ~doc:"beta parameter in moebius transformation"
      and image_w_parameter =
        flag "--image-width" (optional_with_default (-1) int) ~doc:"image-width parameter in moebius transformation"
     (* and remaining_args = anon (sequence ("remaining" %: string)) *)
    in
     fun () ->
       (* calling the main logic of the program *)
       if String.(display_type = "") && Float.(alpha_parameter = -1.) && Float.(beta_parameter = -1.) && image_w_parameter = -1 then
        looping ()
       else
       display_static_image display_type alpha_parameter beta_parameter image_w_parameter
    )

(* the actual main method of the program where the program begins running *)
let () =
  (* calling the command line arguments funciton, command *)
  Command_unix.run ~version:"1.0" ~build_info:"RWO" command
