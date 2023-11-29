(* Intentionally left empty until your implementation *)

open Core
open Math
open Rasterizer
open Ascii_printer

let display_static_image display_type alpha beta img_w : unit =
  match display_type with
  | "planar" ->
      let img =
        getImage ~img_w ~view_size:4 ~plane_bd:4 ~half_edge_length:2
          ~line_w:0.25 ~grid_size:2 Planar ~alpha:(Degree.of_float alpha)
          ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list [ 0.; 0.; 1. ])
      in
      let () = print_ascii_image img img_w in
      ()
  | "sphere" ->
      let img =
        getImage ~img_w ~view_size:4 ~plane_bd:4 ~half_edge_length:2
          ~line_w:0.25 ~grid_size:2 Sphere ~alpha:(Degree.of_float alpha)
          ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list [ 0.; 0.; 1. ])
      in
      let () = print_ascii_image img img_w in
      ()
  | "orthogonal" ->
      let img =
        getImage ~img_w ~view_size:4 ~plane_bd:4 ~half_edge_length:2
          ~line_w:0.25 ~grid_size:2 Orthogonal ~alpha:(Degree.of_float alpha)
          ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list [ 0.; 0.; 1. ])
      in
      let () = print_ascii_image img img_w in
      ()
  | _ ->
      let () =
        print_endline
          "display type command line argument should be one of 'planar', \
           'sphere', or 'orthogonal' "
      in
      ()

let animate_beta ~low ~high frame_rate display_type alpha img_w duration =
  let sleep_time = 1. /. float_of_int frame_rate in
  let betas =
    List.range 0 (int_of_float (float_of_int frame_rate *. duration))
    |> List.map ~f:(fun x ->
           (float_of_int x *. (high -. low) *. sleep_time /. duration) +. low)
  and redraw_and_sleep beta =
    display_static_image display_type alpha beta img_w;
    Caml_unix.sleepf sleep_time
    (* this seems to sleep more than needed, is that a problem in IO or system call? although it's not a big deal so look at it later *)
  in

  List.iter betas ~f:(fun beta -> redraw_and_sleep beta)

let rec looping () =
  Out_channel.output_string stdout "enter something: \n";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> looping ()
  | Some "exit" -> ()
  | Some s -> (
      Out_channel.output_string stdout ("You just input: " ^ s ^ "\n");
      Out_channel.flush stdout;
      match float_of_string_opt s with
      | None ->
          Out_channel.output_string stdout
            "the value cannot be convert to a float\n";
          looping ()
      | Some f ->
          animate_beta ~low:15. ~high:f 30 "orthogonal" 0. 50 1.5;
          looping ())

(* do something here, probably update internal values and redraw the ascii images *)
(* in looping () *)

(* command line argument parsing *)
let command =
  Command.basic
    ~summary:
      "Make an n-gram distribution and allow certain queries to be run, like \
       sample from n-gram distribution to make a sentence"
    (* command line arguments are below *)
    (let%map_open.Command display_type =
       flag "--display-type"
         (optional_with_default "" string)
         ~doc:"The type of display: sphere, planar, or orthogonal"
     and alpha_parameter =
       flag "--alpha"
         (optional_with_default (-1.) float)
         ~doc:"alpha parameter in moebius transformation"
     and beta_parameter =
       flag "--beta"
         (optional_with_default (-1.) float)
         ~doc:"beta parameter in moebius transformation"
     and img_w_parameter =
       flag "--image-width"
         (optional_with_default (-1) int)
         ~doc:"image-width parameter in moebius transformation"
       (* and remaining_args = anon (sequence ("remaining" %: string)) *)
     in
     fun () ->
       (* calling the main logic of the program *)
       if
         String.(display_type = "")
         && Float.(alpha_parameter = -1.)
         && Float.(beta_parameter = -1.)
         && img_w_parameter = -1
       then looping ()
       else
         display_static_image display_type alpha_parameter beta_parameter
           img_w_parameter)

(* the actual main method of the program where the program begins running *)
let () =
  (* calling the command line arguments funciton, command *)
  Command_unix.run ~version:"1.0" ~build_info:"RWO" command
