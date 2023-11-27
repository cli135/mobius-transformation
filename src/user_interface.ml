(* Intentionally left empty until your implementation *)

open Core

let () =
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
  in looping ()