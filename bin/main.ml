(*----------------------------------------------------------------------------*)
(* file  : main.ml                                                            *)
(* brief : application entry point and arg parser code                        *)
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(* Module Loading                                                             *)
(*----------------------------------------------------------------------------*)

open MyUtil

open Token
open MyLexing
open MyParsing
(*
open CodeGen

open CppTemplate
*)
(*----------------------------------------------------------------------------*)
(* External Function loading                                                  *)
(*----------------------------------------------------------------------------*)

external _make_dfa : (MyParsing.flat_regex list * int) list * int -> 
  (int array array) * int array * int * int * int = "MakeDFA"

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
  (*
  let toks = MyLexing.lexAll str in
  let lex_file = MyParsing.parse toks in 
  let flat_cases = MyParsing.flatten_rule lex_file.rule in 
  let alphabet = get_alphabet () in
  let alphabet_size = Array.length alphabet in
  let start, dead, size, ttable , ctable = process_rule flat_cases in
  let _ = Printf.printf "Alphabet size: %d\n" alphabet_size in
  let _ = Printf.printf "Start State: %d\n" start in
  let _ = Printf.printf "Dead State: %d\n" dead in
  let _ = Printf.printf "Number of States: %d\n" size in
  let _ = Printf.printf "Number of Transitions: %d\n" ( size * alphabet_size ) in
  let _ = Printf.printf "ctable size: %d\n" (Array.length ctable) in
  let _ = Printf.printf "ttable size: %d x %d\n" (Array.length ttable) (Array.length ttable.(0)) in
  let gen_context = CppTemplate.context output_filename in
  let src_context = CodeGen.create_source_context start dead size ttable ctable lex_file in
  let _ = CodeGen.generate_code src_context gen_context in
  ()
  *) 
  let toks = MyLexing.tokenize str 0 in
  let rec aux xs = 
    match xs with
    | [] -> ()
    | x :: xs -> 
      let _ = print_endline (Token.string_of_token x) in
      aux xs
  in 
  let _ = aux toks in
  let lexfile = MyParsing.parse toks in 
  let cases = lexfile.rule.cases in 
  let rec aux (cases : MyParsing.case list)= 
    match cases with 
    | [] -> ()
    | x :: xs -> 
      let flat = MyParsing.postorder x.regex in 
      Printf.printf "Regex: %s\n  -> " (MyParsing.string_of_regex x.regex); 
      let strli = List.map MyParsing.string_of_flat_regex flat in
      List.iter print_string strli;
      print_endline "";
      aux xs;
  in
  aux cases;


  ()