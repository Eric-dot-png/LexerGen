(*----------------------------------------------------------------------------*)
(* file  : main.ml                                                            *)
(* brief : application entry point and arg parser code                        *)
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(* Module Loading                                                             *)
(*----------------------------------------------------------------------------*)

open MyLexing
open MyParsing
open MyUtil

(*----------------------------------------------------------------------------*)
(* External Function loading                                                  *)
(*----------------------------------------------------------------------------*)

external process_rule : (string * int * string * string) list -> 
  (int * int * int * (int list list)) = "processRule"

external get_alphabet : unit -> char array = "getAlphabet"

(*----------------------------------------------------------------------------*)
(* Arg Parsing                                                                *)
(*----------------------------------------------------------------------------*)

let parse_args : string * string * bool =
  let input_filename = ref "none" in
  let output_filename = ref "default" in
  let debug_msgs = ref false in
  let arg_specs = [
    ("-i", Arg.Set_string input_filename, "Set input filename (required)");
    ("-o", Arg.Set_string output_filename,"Set output filename (default : <input filename>.cpp/ml)");
    ("-d", Arg.Set debug_msgs,"Set the program to print debug messages");
  ] in
  let usage_msg = "Usage:\ndune exec -- ./bin/main.exe [-i <filename>] [-o <filename>] [-d] <anonymous_args>" in
  let _ = Arg.parse arg_specs
    (fun anon_arg ->
       let _ = Printf.printf "Unexpected argument: \"%s\"\n" anon_arg in
       let _ = Arg.usage arg_specs usage_msg in
       exit 1) usage_msg 
  in 
  !input_filename, !output_filename, !debug_msgs

  
(*----------------------------------------------------------------------------*)
(* Application entry point                                                    *)
(*----------------------------------------------------------------------------*)

let () = 
  let input_filename, output_filename, debug_msgs = parse_args in 
  let _ = Printf.printf "Input Filename: %s\n" input_filename in
  let _ = Printf.printf "Output Filename: %s\n" output_filename in
  let _ = Printf.printf "Debug: %b\n" debug_msgs in
  let str = MyUtil.read_file input_filename in
  let toks = MyLexing.lexAll str in
  let lex_file = MyParsing.parse toks in 
  let _ = MyParsing.print_lex_file lex_file in
  let _, flat_cases = MyParsing.flatten_rule lex_file.rule in 
  let alphabet = get_alphabet () in
  let _ = Printf.printf "Alphabet Size: %d\n" (Array.length alphabet) in
  let start, dead, size, _ = process_rule flat_cases in 
  let _ = Printf.printf "Start State: %d\n" start in
  let _ = Printf.printf "Dead State: %d\n" dead in
  let _ = Printf.printf "Number of States: %d\n" size in
  let _ = Printf.printf "Number of Transitions: %d\n" ( (size-1) * (Array.length alphabet) ) in
  () 