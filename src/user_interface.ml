open Core
open Math
open Rasterizer
open Ascii_printer
open Animation
open Yojson.Basic.Util

let helper_page_filename = "help.txt"
let helper_page = Stdio.In_channel.read_all helper_page_filename

(* parsing input commands *)
let get_render_mode s =
  match String.lowercase s with
  | "orthogonal" -> Some Orthogonal
  | "planar" -> Some Planar
  | "sphere" -> Some Sphere
  | _ -> None

(* struct params{
     frames:...

     val load_default_param
     val set_params:
   }

   end *)


module Params = struct
  (* mutable state references for parameters the user can change *)
  (* initialize mutable references with defaults, will change later based on what is
     loaded in from config.json *)
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
  let current_supersampling = ref false

  

  (* functions *)
  (* called when the program (i.e. this module) loads and when the 'reset' command is typed *)
  let load_default_param () : unit =
    (* json *)
    let config_filename = "config.json" in
    let json = Yojson.Basic.from_file config_filename in
    current_alpha := Degree.of_float (json |> member "default_alpha" |> to_float);
    current_beta := Degree.of_float (json |> member "default_beta" |> to_float);
    (current_render_mode :=
       json |> member "default_render_mode" |> to_string |> get_render_mode
       |> function
       | Some s -> s
       | None ->
           Out_channel.output_string stdout
             "Must be one of Orthogonal, Planar, or Sphere; defaulting to \
              Orthogonal";
           Orthogonal);
    current_center :=
      Vec3.of_list
        (json |> member "default_center" |> to_list |> List.map ~f:to_float);
    current_img_w := json |> member "default_img_w" |> to_int;
    current_view_size := json |> member "default_view_size" |> to_int;
    current_plane_bd := json |> member "default_plane_bd" |> to_int;
    current_half_edge_length :=
      json |> member "default_half_edge_length" |> to_int;
    current_line_w := json |> member "default_line_w" |> to_float;
    current_grid_size := json |> member "default_grid_size" |> to_int;
    current_frame_rate := json |> member "default_frame_rate" |> to_int;
    current_duration := json |> member "default_duration" |> to_float;
    current_supersampling := json |> member "default_supersampling" |> to_bool;
    Out_channel.output_string stdout "reset all parameters\n"

  (* sets parameters when set commands are called by the user *)
  let set_param ref_variable (variable_name : string) cast_opt
      (variable_type_string : string) paramvalue redraw looping =
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
end

(* these parameters are here mainly for doing animation easily *)
let redraw render_mode alpha beta center : unit =
  match render_mode with
  | Planar ->
      let img =
        getImage ~img_w:!Params.current_img_w
          ~view_size:!Params.current_view_size
          ~plane_bd:!Params.current_plane_bd
          ~half_edge_length:!Params.current_half_edge_length
          ~line_w:!Params.current_line_w ~grid_size:!Params.current_grid_size
          ~sampling_n:(if !Params.current_supersampling then 4 else 1)
          Planar ~alpha ~beta ~center
      in
      let () = print_ascii_image img !Params.current_img_w in
      ()
  | Sphere ->
      let img =
        getImage ~img_w:!Params.current_img_w
          ~view_size:!Params.current_view_size
          ~plane_bd:!Params.current_plane_bd
          ~half_edge_length:!Params.current_half_edge_length
          ~line_w:!Params.current_line_w ~grid_size:!Params.current_grid_size
          ~sampling_n:(if !Params.current_supersampling then 4 else 1)
          Sphere ~alpha ~beta ~center
      in
      let () = print_ascii_image img !Params.current_img_w in
      ()
  | Orthogonal ->
      let img =
        getImage ~img_w:!Params.current_img_w
          ~view_size:!Params.current_view_size
          ~plane_bd:!Params.current_plane_bd
          ~half_edge_length:!Params.current_half_edge_length
          ~line_w:!Params.current_line_w ~grid_size:!Params.current_grid_size
          ~sampling_n:(if !Params.current_supersampling then 4 else 1)
          Orthogonal ~alpha ~beta ~center
      in
      let () = print_ascii_image img !Params.current_img_w in
      ()

(* this seems to sleep more than needed, is that a problem in IO or system call? although it's not a big deal so look at it later *)
let show_animation render_mode frame_rate keyframes_list =
  let sleep_time = 1. /. float_of_int frame_rate in
  let redraw_and_sleep { alpha; beta; center } =
    redraw render_mode alpha beta center;
    Caml_unix.sleepf sleep_time
  in
  List.iter keyframes_list ~f:redraw_and_sleep

let cool_animation =
  get_cool_animation !Params.current_frame_rate !Params.current_duration

let rec looping () =
  Out_channel.output_string stdout "enter your command: \n";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> looping ()
  | Some "exit" -> ()
  | Some "reset" ->
      Params.load_default_param ();
      redraw
        !Params.current_render_mode
        !Params.current_alpha !Params.current_beta !Params.current_center;
      looping ()
  | Some "help" ->
      Out_channel.output_string stdout helper_page;
      looping ()
  | Some "cool" ->
      (* the frames in this animation is fixed. If the user wants to play it slower they need to decrease the framerate *)
      show_animation
        !Params.current_render_mode
        !Params.current_frame_rate cool_animation;
      looping ()
  | Some s -> (
      Out_channel.flush stdout;
      match s with _ -> parse_command_strings_in_loop s)

and parse_command_strings_in_loop (s : string) =
  let tokens_in_command_string =
    String.split_on_chars s ~on:[ ' '; '\t'; '\n' ]
    |> List.filter ~f:(fun x -> String.(x <> ""))
  in
  match tokens_in_command_string with
  | [ "set"; "alpha"; final_value ] -> (
      match float_of_string_opt final_value with
      | Some d ->
          let new_alpha = Degree.of_float d in
          generate_keyframes
            {
              alpha = !Params.current_alpha;
              beta = !Params.current_beta;
              center = !Params.current_center;
            }
            {
              alpha = new_alpha;
              beta = !Params.current_beta;
              center = !Params.current_center;
            }
            !Params.current_frame_rate !Params.current_duration
            linear_interpolate
          |> show_animation
               !Params.current_render_mode
               !Params.current_frame_rate;
          Params.current_alpha := new_alpha;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  (* this is just one additional value to add to to alpha *)
  | [ "add"; "alpha"; additional_alpha ] -> (
      match float_of_string_opt additional_alpha with
      | Some add_v ->
          let new_alpha =
            Degree.( + ) !Params.current_alpha (Degree.of_float add_v)
          in
          generate_keyframes
            {
              alpha = !Params.current_alpha;
              beta = !Params.current_beta;
              center = !Params.current_center;
            }
            {
              alpha = new_alpha;
              beta = !Params.current_beta;
              center = !Params.current_center;
            }
            !Params.current_frame_rate !Params.current_duration
            linear_interpolate
          |> show_animation
               !Params.current_render_mode
               !Params.current_frame_rate;
          Params.current_alpha := new_alpha;
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
              alpha = !Params.current_alpha;
              beta = !Params.current_beta;
              center = !Params.current_center;
            }
            {
              alpha = !Params.current_alpha;
              beta = new_beta;
              center = !Params.current_center;
            }
            !Params.current_frame_rate !Params.current_duration
            linear_interpolate
          |> show_animation
               !Params.current_render_mode
               !Params.current_frame_rate;
          Params.current_beta := new_beta;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  (* this is just one additional value to add to to alpha *)
  | [ "add"; "beta"; additional_beta ] -> (
      match float_of_string_opt additional_beta with
      | Some add_v ->
          let new_beta =
            Degree.( + ) !Params.current_beta (Degree.of_float add_v)
          in
          generate_keyframes
            {
              alpha = !Params.current_alpha;
              beta = !Params.current_beta;
              center = !Params.current_center;
            }
            {
              alpha = !Params.current_alpha;
              beta = new_beta;
              center = !Params.current_center;
            }
            !Params.current_frame_rate !Params.current_duration
            linear_interpolate
          |> show_animation
               !Params.current_render_mode
               !Params.current_frame_rate;
          Params.current_beta := new_beta;
          looping ()
      | None ->
          Out_channel.output_string stdout
            "The values entered cannot be convert to the float type\n";
          looping ())
  (* the cases below are more mutations of ref cells *)
  | [ "view"; render_mode ] -> (
      match get_render_mode render_mode with
      | Some i ->
          Params.current_render_mode := i;
          Out_channel.output_string stdout
            ("render mode updated to " ^ render_mode ^ "\n");
          redraw
            !Params.current_render_mode
            !Params.current_alpha !Params.current_beta !Params.current_center;
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
                alpha = !Params.current_alpha;
                beta = !Params.current_beta;
                center = !Params.current_center;
              }
              {
                alpha = !Params.current_alpha;
                beta = !Params.current_beta;
                center = new_center;
              }
              !Params.current_frame_rate !Params.current_duration
              linear_interpolate
            |> show_animation
                 !Params.current_render_mode
                 !Params.current_frame_rate;
            Params.current_center := new_center;
            Out_channel.output_string stdout
              ("center set to " ^ x ^ " " ^ y ^ " " ^ z ^ "\n");
            looping ()
      | _, _, _ ->
          Out_channel.output_string stdout
            "one of the values entered cannot be convert to the proper data type\n\
             they should be float float float\n";
          looping ())
  | [ "set"; "img_w"; paramvalue ] ->
      Params.set_param Params.current_img_w "img_w" int_of_string_opt "int"
        paramvalue redraw looping
  | [ "set"; "view_size"; paramvalue ] ->
      Params.set_param Params.current_view_size "view_size" int_of_string_opt
        "int" paramvalue redraw looping
  | [ "set"; "plane_bd"; paramvalue ] ->
      Params.set_param Params.current_plane_bd "plane_bd" int_of_string_opt
        "int" paramvalue redraw looping
  | [ "set"; "half_edge_length"; paramvalue ] ->
      Params.set_param Params.current_half_edge_length "half_edge_length"
        int_of_string_opt "int" paramvalue redraw looping
  | [ "set"; "grid_size"; paramvalue ] ->
      Params.set_param Params.current_grid_size "grid_size" int_of_string_opt
        "int" paramvalue redraw looping
  | [ "set"; "line_w"; paramvalue ] ->
      Params.set_param Params.current_line_w "line_w" float_of_string_opt
        "float" paramvalue redraw looping
  | [ "set"; "frame_rate"; paramvalue ] ->
      Params.set_param Params.current_frame_rate "frame_rate" int_of_string_opt
        "int" paramvalue redraw looping
  | [ "set"; "duration"; paramvalue ] ->
      Params.set_param Params.current_duration "duration" float_of_string_opt
        "float" paramvalue redraw looping
  | [ "set"; "supersampling"; paramvalue ] ->
      Params.set_param Params.current_supersampling "supersampling"
        bool_of_string_opt "bool" paramvalue redraw looping
  | _ ->
      Out_channel.output_string stdout
        "unrecognized command, type help to see the user manual\n";
      looping ()

(* the actual main method of the program where the program begins running *)
let () =
  redraw
    !Params.current_render_mode
    !Params.current_alpha !Params.current_beta !Params.current_center;
  looping ()
