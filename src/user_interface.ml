(* Intentionally left empty until your implementation *)

open Core
open Math
open Rasterizer
open Ascii_printer

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

(* keyframe animation *)
type keyframe_params = { alpha : Degree.t; beta : Degree.t; center : Vec3.t }

let linear_interpolate (k1 : keyframe_params) (k2 : keyframe_params) (t : float)
    : keyframe_params =
  let alpha =
    Degree.( - ) k2.alpha k1.alpha |> Degree.( * ) t |> Degree.( + ) k1.alpha
  and beta =
    Degree.( - ) k2.beta k1.beta |> Degree.( * ) t |> Degree.( + ) k1.beta
  and center =
    Vec3.( - ) k2.center k1.center |> Vec3.( * ) t |> Vec3.( + ) k1.center
  in
  { alpha; beta; center }

let generate_keyframes k1 k2 frame_rate duration interpolation_method =
  let total_frames = int_of_float (float_of_int frame_rate *. duration) in
  List.range 0 (total_frames + 1)
  |> List.map ~f:(fun t ->
         interpolation_method k1 k2 (float_of_int t /. float_of_int total_frames))

(* this seems to sleep more than needed, is that a problem in IO or system call? although it's not a big deal so look at it later *)
let show_animation render_mode frame_rate keyframes_list =
  let sleep_time = 1. /. float_of_int frame_rate in
  let redraw_and_sleep { alpha; beta; center } =
    redraw render_mode alpha beta center;
    Caml_unix.sleepf sleep_time
  in
  List.iter keyframes_list ~f:redraw_and_sleep

(* the following part is for the hardcoded animation *)

(* this arc is a special one so I am just going to hard code it *)
let keyframe_list_arc ?(r = 2.) frame_rate duration change_angle =
  let total_frames = int_of_float (float_of_int frame_rate *. duration) in
  let half_frames = total_frames / 2 in
  let get_location i =
    if i < half_frames then
      let theta =
        float_of_int i /. float_of_int half_frames /. 2. *. Float.pi
      in
      Vec3.of_list [ r *. Float.sin theta; r -. (r *. Float.cos theta); 1. ]
    else
      let theta =
        float_of_int (i - half_frames)
        /. float_of_int half_frames /. 2. *. Float.pi
      in
      Vec3.of_list [ r -. (r *. Float.sin theta); r *. Float.cos theta; 1. ]
  in
  let get_params i =
    if change_angle then
      {
        alpha =
          Degree.of_float
          @@ (180. /. float_of_int total_frames *. float_of_int i);
        beta =
          Degree.of_float
          @@ (360. /. float_of_int total_frames *. float_of_int i);
        center = get_location i;
      }
    else
      {
        alpha = Degree.of_float 0.;
        beta = Degree.of_float 0.;
        center = get_location i;
      }
  in
  List.map ~f:get_params (List.range 0 (total_frames + 1))

(* hard coded animation to play *)
let cool_animation =
  List.concat
    [
      keyframe_list_arc !current_frame_rate !current_duration false;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1. ];
        }
        {
          alpha = Degree.of_float 180.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1. ];
        }
        !current_frame_rate (!current_duration *. 0.8) linear_interpolate;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1. ];
        }
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 4. ];
        }
        !current_frame_rate (!current_duration *. 0.4) linear_interpolate;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 4. ];
        }
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1.5 ];
        }
        !current_frame_rate (!current_duration *. 0.4) linear_interpolate;
      generate_keyframes
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 0.;
          center = Vec3.of_list [ 0.; 0.; 1.5 ];
        }
        {
          alpha = Degree.of_float 0.;
          beta = Degree.of_float 360.;
          center = Vec3.of_list [ 0.; 0.; 1.5 ];
        }
        !current_frame_rate !current_duration linear_interpolate;
      keyframe_list_arc !current_frame_rate (!current_duration *. 2.) true;
    ]

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

(* command line argument parsing *)
(*
let command =
  Command.basic
    ~summary:
      "Accept command line arguments for the moebius transformation user \
       interface"
    (* command line arguments are below *)
    (let%map_open.Command render_mode =
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
         String.(render_mode = "")
         && Float.(alpha_parameter = -1.)
         && Float.(beta_parameter = -1.)
         && img_w_parameter = -1
       then looping ()
       else
         redraw (get_render_mode render_mode) alpha_parameter beta_parameter
           ~center:(Vec3.of_list [ 0.; 0.; 1. ]))
            *)

(* calling the command line arguments funciton, command *)
(* Command_unix.run ~version:"1.0" ~build_info:"RWO" command *)
