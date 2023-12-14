(* Intentionally left empty until your implementation *)

open Core
open Math
open Rasterizer
open Ascii_printer
open Animation

let helper_page =
  "\n\
  \  How to use the user interface:\n\n\
  \  set [alpha/beta] [angle] : set alpha/beta to the input angle in degree\n\
  \    set alpha 90\n\
  \    set beta 180\n\n\
  \  add [alpha/beta] [angle] : increment current alpha/beta by the input \
   angle in degree \n\
  \    add alpha 15\n\
  \    add beta -10\n\n\
  \  view [Sphere/Planar/Orthogonal] : change render views\n\
  \    view Sphere\n\n\
  \  set center [xfloat] [yfloat] [zfloat] : set the sphere center to a new \
   location, zfloat must be a positive value.\n\
  \    move center 0. 1. 3.\n\n\
  \  set [paramname] [paramvalue] : set all the customizable parameters for \
   the viewport\n\
  \    set img_w 100\n\
  \    set view_size 4\n\
  \    set plane_bd 4\n\
  \    set half_edge_length 2\n\
  \    set line_w 0.25\n\
  \    set grid_size 2\n\
  \    set frame_rate 30\n\
  \    set duration 2.\n\n\
  \  cool : this will play a cool animation :)\n\n\
  \  reset: reset all parameters\n\n\
  \  exit: exit the program\n"

(* Todo: add explanation for set  *)
(* mutable state references for parameters the user can change *)
let current_alpha = ref (Degree.of_float 0.0)
let current_beta = ref (Degree.of_float 0.0)
let current_render_mode = ref Orthogonal
let current_center = ref (Vec3.of_list [ 0.; 0.; 1. ])
let current_img_w = ref 50
let current_view_size = ref 4
let current_plane_bd = ref 4
let current_half_edge_length = ref 2
let current_line_w = ref 0.25
let current_grid_size = ref 2
let current_frame_rate = ref 30
let current_duration = ref 1.5

(* these parameters are here mainly for doing animation easily *)
let redraw render_mode alpha beta center : unit =
  match render_mode with
  | Planar ->
      let img =
        getImage ~img_w:!current_img_w ~view_size:!current_view_size
          ~plane_bd:!current_plane_bd
          ~half_edge_length:!current_half_edge_length ~line_w:!current_line_w
          ~grid_size:!current_grid_size Planar ~alpha ~beta ~center
      in
      let () = print_ascii_image img !current_img_w in
      ()
  | Sphere ->
      let img =
        getImage ~img_w:!current_img_w ~view_size:!current_view_size
          ~plane_bd:!current_plane_bd
          ~half_edge_length:!current_half_edge_length ~line_w:!current_line_w
          ~grid_size:!current_grid_size Sphere ~alpha ~beta ~center
      in
      let () = print_ascii_image img !current_img_w in
      ()
  | Orthogonal ->
      let img =
        getImage ~img_w:!current_img_w ~view_size:!current_view_size
          ~plane_bd:!current_plane_bd
          ~half_edge_length:!current_half_edge_length ~line_w:!current_line_w
          ~grid_size:!current_grid_size Orthogonal ~alpha ~beta ~center
      in
      let () = print_ascii_image img !current_img_w in
      ()



(* this seems to sleep more than needed, is that a problem in IO or system call? although it's not a big deal so look at it later *)
let show_animation render_mode frame_rate keyframes_list =
  let sleep_time = 1. /. float_of_int frame_rate in
  let redraw_and_sleep { alpha; beta; center } =
    redraw render_mode alpha beta center;
    Caml_unix.sleepf sleep_time
  in
  List.iter keyframes_list ~f:redraw_and_sleep

let cool_animation = get_cool_animation !current_frame_rate !current_duration

(* parsing input commands *)
let get_render_mode s =
  match String.lowercase s with
  | "orthogonal" -> Some Orthogonal
  | "planar" -> Some Planar
  | "sphere" -> Some Sphere
  | _ -> None

let rec looping () =
  Out_channel.output_string stdout "enter your command: \n";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> looping ()
  | Some "exit" -> ()
  | Some "reset" ->
      current_alpha := Degree.of_float 0.0;
      current_beta := Degree.of_float 0.0;
      current_render_mode := Orthogonal;
      current_center := Vec3.of_list [ 0.; 0.; 1. ];
      current_img_w := 50;
      current_view_size := 4;
      current_plane_bd := 4;
      current_half_edge_length := 2;
      current_line_w := 0.25;
      current_grid_size := 2;
      current_frame_rate := 30;
      current_duration := 1.5;
      Out_channel.output_string stdout "reset all parameters\n";
      redraw !current_render_mode !current_alpha !current_beta !current_center;
      looping ()
  | Some "help" ->
      Out_channel.output_string stdout helper_page;
      looping ()
  | Some "cool" ->
      (* the frames in this animation is fixed. If the user wants to play it slower they need to decrease the framerate *)
      show_animation !current_render_mode !current_frame_rate cool_animation;
      looping ()
  | Some s -> (
      Out_channel.flush stdout;
      match s with _ -> parse_command_strings_in_loop s)

and parse_command_strings_in_loop (s : string) =
  let tokens_in_command_string =
    String.split_on_chars s ~on:[ ' '; '\t'; '\n' ]
    |> List.filter ~f:(fun x -> String.(x <> ""))
  in
  (* this part could be refactored to do a Some/None monad I think
     since it involves nested pattern matching cases on Some and None, with
     early exits for None, just a guess *)
  let set_command ref_variable (variable_name : string) cast_opt
      (variable_type_string : string) paramvalue =
    match cast_opt paramvalue with
    | Some i ->
        ref_variable := i;
        Out_channel.output_string stdout
          (variable_name ^ " updated to " ^ paramvalue ^ "\n");
        redraw !current_render_mode !current_alpha !current_beta !current_center;
        looping ()
    | _ ->
        Out_channel.output_string stdout
          ("one of the values entered cannot be convert to the proper data type\n\
            they should be " ^ variable_type_string ^ "\n");
        looping ()
  in
  match tokens_in_command_string with
  | [ "set"; "alpha"; final_value ] -> (
      match float_of_string_opt final_value with
      | Some d ->
          let new_alpha = Degree.of_float d in
          generate_keyframes
            {
              alpha = !current_alpha;
              beta = !current_beta;
              center = !current_center;
            }
            {
              alpha = new_alpha;
              beta = !current_beta;
              center = !current_center;
            }
            !current_frame_rate !current_duration linear_interpolate
          |> show_animation !current_render_mode !current_frame_rate;
          current_alpha := new_alpha;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  (* this is just one additional value to add to to alpha *)
  | [ "add"; "alpha"; additional_alpha ] -> (
      match float_of_string_opt additional_alpha with
      | Some add_v ->
          let new_alpha = Degree.( + ) !current_alpha (Degree.of_float add_v) in
          generate_keyframes
            {
              alpha = !current_alpha;
              beta = !current_beta;
              center = !current_center;
            }
            {
              alpha = new_alpha;
              beta = !current_beta;
              center = !current_center;
            }
            !current_frame_rate !current_duration linear_interpolate
          |> show_animation !current_render_mode !current_frame_rate;
          current_alpha := new_alpha;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  | [ "set"; "beta"; final_value ] -> (
      match float_of_string_opt final_value with
      | Some d ->
          let new_beta = Degree.of_float d in
          generate_keyframes
            {
              alpha = !current_alpha;
              beta = !current_beta;
              center = !current_center;
            }
            {
              alpha = !current_alpha;
              beta = new_beta;
              center = !current_center;
            }
            !current_frame_rate !current_duration linear_interpolate
          |> show_animation !current_render_mode !current_frame_rate;
          current_beta := new_beta;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  (* this is just one additional value to add to to alpha *)
  | [ "add"; "beta"; additional_beta ] -> (
      match float_of_string_opt additional_beta with
      | Some add_v ->
          let new_beta = Degree.( + ) !current_beta (Degree.of_float add_v) in
          generate_keyframes
            {
              alpha = !current_alpha;
              beta = !current_beta;
              center = !current_center;
            }
            {
              alpha = !current_alpha;
              beta = new_beta;
              center = !current_center;
            }
            !current_frame_rate !current_duration linear_interpolate
          |> show_animation !current_render_mode !current_frame_rate;
          current_beta := new_beta;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  (* the cases below are more mutations of ref cells *)
  | [ "view"; render_mode ] -> (
      match get_render_mode render_mode with
      | Some i ->
          current_render_mode := i;
          Out_channel.output_string stdout
            ("render mode updated to " ^ render_mode ^ "\n");
          redraw !current_render_mode !current_alpha !current_beta
            !current_center;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "unrecognized render mode. Should be one of \"sphere\" \
             \"orthogonal\" \"planar\"\n";
          looping ())
  | [ "set"; "center"; x; y; z ] -> (
      match
        (float_of_string_opt x, float_of_string_opt y, float_of_string_opt z)
      with
      | Some x_float, Some y_float, Some z_float ->
          if Float.( < ) z_float 0. then (
            Out_channel.output_string stdout
              "Invalid value: z-axis value of center must by greater than 0\n";
            looping ())
          else
            let new_center = Vec3.of_list [ x_float; y_float; z_float ] in
            generate_keyframes
              {
                alpha = !current_alpha;
                beta = !current_beta;
                center = !current_center;
              }
              {
                alpha = !current_alpha;
                beta = !current_beta;
                center = new_center;
              }
              !current_frame_rate !current_duration linear_interpolate
            |> show_animation !current_render_mode !current_frame_rate;
            current_center := new_center;
            Out_channel.output_string stdout
              ("center set to " ^ x ^ " " ^ y ^ " " ^ z ^ "\n");
            looping ()
      | _, _, _ ->
          Out_channel.output_string stdout
            "one of the values entered cannot be convert to the proper data type\n\
             they should be float float float\n";
          looping ())
  | [ "set"; "img_w"; paramvalue ] ->
      set_command current_img_w "img_w" int_of_string_opt "int" paramvalue
  | [ "set"; "view_size"; paramvalue ] ->
      set_command current_view_size "view_size" int_of_string_opt "int"
        paramvalue
  | [ "set"; "plane_bd"; paramvalue ] ->
      set_command current_plane_bd "plane_bd" int_of_string_opt "int" paramvalue
  | [ "set"; "half_edge_length"; paramvalue ] ->
      set_command current_half_edge_length "half_edge_length" int_of_string_opt
        "int" paramvalue
  | [ "set"; "grid_size"; paramvalue ] ->
      set_command current_grid_size "grid_size" int_of_string_opt "int"
        paramvalue
  | [ "set"; "line_w"; paramvalue ] ->
      set_command current_line_w "line_w" float_of_string_opt "float" paramvalue
  | [ "set"; "frame_rate"; paramvalue ] ->
      set_command current_frame_rate "frame_rate" int_of_string_opt "int"
        paramvalue
  | [ "set"; "duration"; paramvalue ] ->
      set_command current_duration "duration" float_of_string_opt "float"
        paramvalue
  | _ ->
      Out_channel.output_string stdout
        "unrecognized command, type help to see the user manual\n";
      looping ()

(* the actual main method of the program where the program begins running *)
let () =
  redraw !current_render_mode !current_alpha !current_beta !current_center;
  looping ()
