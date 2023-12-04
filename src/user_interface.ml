(* Intentionally left empty until your implementation *)

open Core
open Math
open Rasterizer
open Ascii_printer

(* 
How to run the user interface:

animate [alphaorbeta] [low] [high] [framerate] [duration]
  animate alpha 15. 30. 30 1.5
  animate beta 15. 30. 30 1.5

  animate [alphaorbeta] [additionalamount] [framerate] [duration]
  animate alpha 15. 30 1.5
  animate beta 15. 30 1.5

change view to [displaytype]
  change view to sphere
  change view to planar
  change view to orthogonal

move center [xfloat] [yfloat] [zfloat]
  move center 0. 1. 3.

set [paramname] [paramvalueintorfloat?]
  set img_w 100
  set view_size 4
  set plane_bd 4
  set half_edge_length 2
  set line_w 0.25
  set grid_size 2

*)

(* mutable state references for parameters the user can change *)
let current_alpha = ref 0.0
let current_beta = ref 0.0
(* let current_framerate = ref 30 *)
let current_displaytype = ref "orthogonal"
let current_center = ref [ 0.; 0.; 1. ]
let current_img_w = ref 50
let current_view_size = ref 4
let current_plane_bd = ref 4
let current_half_edge_length = ref 2
let current_line_w = ref 0.25
let current_grid_size = ref 2

let display_static_image display_type alpha beta img_w : unit =
  match display_type with
  | "planar" ->
      let img =
        getImage ~img_w ~view_size:!current_view_size ~plane_bd:!current_plane_bd ~half_edge_length:!current_half_edge_length
          ~line_w:!current_line_w ~grid_size:!current_grid_size Planar ~alpha:(Degree.of_float alpha)
          ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list !current_center)
      in
      let () = print_ascii_image img img_w in
      ()
  | "sphere" ->
      let img =
        getImage ~img_w ~view_size:!current_view_size ~plane_bd:!current_plane_bd ~half_edge_length:!current_half_edge_length
          ~line_w:!current_line_w ~grid_size:!current_grid_size Sphere ~alpha:(Degree.of_float alpha)
          ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list !current_center)
      in
      let () = print_ascii_image img img_w in
      ()
  | "orthogonal" ->
      let img =
        getImage ~img_w ~view_size:!current_view_size ~plane_bd:!current_plane_bd ~half_edge_length:!current_half_edge_length
          ~line_w:!current_line_w ~grid_size:!current_grid_size Orthogonal ~alpha:(Degree.of_float alpha)
          ~beta:(Degree.of_float beta)
          ~center:(Vec3.of_list !current_center)
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

(* analogous animation for alpha *)
let animate_alpha ~low ~high frame_rate display_type beta img_w duration =
  let sleep_time = 1. /. float_of_int frame_rate in
  let alphas =
    List.range 0 (int_of_float (float_of_int frame_rate *. duration))
    |> List.map ~f:(fun x ->
            (float_of_int x *. (high -. low) *. sleep_time /. duration) +. low)
  and redraw_and_sleep alpha =
    display_static_image display_type alpha beta img_w;
    Caml_unix.sleepf sleep_time
    (* this seems to sleep more than needed, is that a problem in IO or system call? although it's not a big deal so look at it later *)
  in

  List.iter alphas ~f:(fun alpha -> redraw_and_sleep alpha)



let rec looping () =
  Out_channel.output_string stdout "enter something: \n";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> looping ()
  | Some "exit" -> ()
  | Some s -> (
      Out_channel.output_string stdout ("You just input: " ^ s ^ "\n");
      Out_channel.flush stdout;
      parse_command_strings_in_loop s
  )
and parse_command_strings_in_loop (s : string) =
  let tokens_in_command_string =
    String.split_on_chars s ~on:[' '; '\t'; '\n']
    |> List.filter ~f:(fun x -> String.(x <> ""))
  in
  (* this part could be refactored to do a Some/None monad I think
     since it involves nested pattern matching cases on Some and None, with
     early exits for None, just a guess *)
  match tokens_in_command_string with
  | "animate" :: "alpha" :: low :: high :: framerate :: duration :: [] ->
    begin
    match float_of_string_opt low,
          float_of_string_opt high,
          int_of_string_opt framerate,
          float_of_string_opt duration
    with
    | Some l, Some h, Some frmrt, Some dur ->
      (* add mutable state for the display type, center, and current alpha and beta stored low
          starting points. Also add state for all of the ones in set img_w 100 or related parameters
          *)
      animate_alpha ~low:l ~high:h frmrt !current_displaytype !current_beta !current_img_w dur;
      current_alpha := h;
      looping ()
    | _, _, _, _ -> 
        Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be float float int float\n";
        looping ()
    end
  (* this is just one additional value to add to to alpha *)
  | "animate" :: "alpha" :: additional_alpha :: framerate :: duration :: [] ->
      begin
      match float_of_string_opt additional_alpha,
            int_of_string_opt framerate,
            float_of_string_opt duration
      with
      | Some addl, Some frmrt, Some dur ->
        (* add mutable state for the display type, center, and current alpha and beta stored low
            starting points. Also add state for all of the ones in set img_w 100 or related parameters
            *)
        animate_alpha ~low:!current_alpha ~high:((!current_alpha) +. addl) frmrt !current_displaytype !current_beta !current_img_w dur;
        (* update the current alpha *)
        current_alpha := !current_alpha +. addl;
        looping ()
      | _, _, _ -> 
          Out_channel.output_string stdout
            "one of the values entered cannot be convert to the proper data type\nthey should be float int float\n";
          looping ()
      end  
  | "animate" :: "beta" :: low :: high :: framerate :: duration :: [] ->
    begin
    match float_of_string_opt low,
            float_of_string_opt high,
            int_of_string_opt framerate,
            float_of_string_opt duration
      with
      | Some l, Some h, Some frmrt, Some dur ->
        (* add mutable state for the display type, center, and current alpha and beta stored low
           starting points. Also add state for all of the ones in set img_w 100 or related parameters
            *)
        animate_beta ~low:l ~high:h frmrt !current_displaytype !current_beta !current_img_w dur;
        current_beta := h;
        looping ()
      | _, _, _, _ -> 
          Out_channel.output_string stdout
            "one of the values entered cannot be convert to the proper data type\nthey should be float float int float\n";
          looping ()
    end
    (* this is just one additional value to add to to alpha *)
  | "animate" :: "beta" :: additional_beta :: framerate :: duration :: [] ->
    begin
    match float_of_string_opt additional_beta,
          int_of_string_opt framerate,
          float_of_string_opt duration
    with
    | Some addl, Some frmrt, Some dur ->
      (* add mutable state for the display type, center, and current alpha and beta stored low
          starting points. Also add state for all of the ones in set img_w 100 or related parameters
          *)
      animate_beta ~low:!current_beta ~high:((!current_beta) +. addl) frmrt !current_displaytype !current_alpha !current_img_w dur;
      (* update the current beta *)
      current_beta := !current_beta +. addl;
      looping ()
    | _, _, _ -> 
        Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be float int float\n";
        looping ()
    end  
  (* the cases below are more mutations of ref cells *)
  | "change" :: "view" :: "to" :: displaytype :: [] ->
    current_displaytype := displaytype;
    Out_channel.output_string stdout ("displaytype updated to " ^ displaytype ^ "\n");
    looping ()
  | "move" :: "center" :: x :: y:: z :: [] ->
    begin
    match float_of_string_opt x, float_of_string_opt y, float_of_string_opt z with
    | Some x_float, Some y_float, Some z_float ->
      current_center := [x_float; y_float; z_float];
      Out_channel.output_string stdout ("center moved to " ^ x ^ " " ^ y ^ " " ^ z ^ "\n");
      looping ()
    | _, _, _ ->
      Out_channel.output_string stdout
        "one of the values entered cannot be convert to the proper data type\nthey should be float float float\n";
      looping ()
    end
    | "set" :: "img_w" :: paramvalue :: [] ->
      begin
      match int_of_string_opt paramvalue with
      | Some i ->
        current_img_w := i;
        Out_channel.output_string stdout ("img_w updated to " ^ paramvalue ^ "\n");
        looping ()
      | _ ->
        Out_channel.output_string stdout
        "one of the values entered cannot be convert to the proper data type\nthey should be int\n";
        looping ()
      end
    | "set" :: "view_size" :: paramvalue :: [] ->
      begin
        match int_of_string_opt paramvalue with
        | Some i ->
          current_view_size := i;
          Out_channel.output_string stdout ("view_size updated to " ^ paramvalue);
          looping ()
        | _ ->
          Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be int\n";
          looping ()
      end  
    | "set" :: "plane_bd" :: paramvalue :: [] ->
      begin
        match int_of_string_opt paramvalue with
        | Some i ->
          current_plane_bd := i;
          Out_channel.output_string stdout ("plane_bd updated to " ^ paramvalue ^ "\n");
          looping ()
        | _ ->
          Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be int\n";
          looping ()
      end
    | "set" :: "half_edge_length" :: paramvalue :: [] ->
      begin
        match int_of_string_opt paramvalue with
        | Some i ->
          current_half_edge_length := i;
          Out_channel.output_string stdout ("half_edge_length updated to " ^ paramvalue ^ "\n");
          looping ()
        | _ ->
          Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be int\n";
          looping ()
      end
    | "set" :: "grid_size" :: paramvalue :: [] ->
      begin
        match int_of_string_opt paramvalue with
        | Some i ->
          current_grid_size := i;
          Out_channel.output_string stdout ("grid_size updated to " ^ paramvalue ^ "\n");
          looping ()
        | _ ->
          Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be int\n";
          looping ()
      end
    | "set" :: "line_w" :: paramvalue :: [] ->
      begin
        match float_of_string_opt paramvalue with
        | Some f ->
          current_line_w := f;
          Out_channel.output_string stdout ("line_w updated to " ^ paramvalue ^ "\n");
          looping ()
        | _ ->
          Out_channel.output_string stdout
          "one of the values entered cannot be convert to the proper data type\nthey should be float\n";
          looping ()
      end
  | _ ->
    Out_channel.output_string stdout
      ("the command is not one of the possible commands: animate, change view to, move, set\n");
    looping ()

  (* match float_of_string_opt s with
      | None ->
          Out_channel.output_string stdout
            "the value cannot be convert to a float\n";
          looping ()
      | Some f ->
          animate_beta ~low:15. ~high:f 30 "orthogonal" 0. 50 1.5;
          looping () *)

(* do something here, probably update internal values and redraw the ascii images *)
(* in looping () *)

(* command line argument parsing *)
let command =
  Command.basic
    ~summary:
      "Accept command line arguments for the moebius transformation user interface"
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
