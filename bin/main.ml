(*----------------------------------------------------------------------------*)
(* file  : main.ml                                                            *)
(* brief : application entry point and arg parser code                        *)
(*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*)
(* Module Loading                                                             *)
(*----------------------------------------------------------------------------*)

open MyUtil
open MyLexing
open MyParsing
open CodeGen
open CppTemplate

(*----------------------------------------------------------------------------*)
(* External Function loading                                                  *)
(*----------------------------------------------------------------------------*)

external _make_dfa : (MyParsing.flat_regex list * int) list -> int -> int ->
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
  Printf.printf "Input Filename: %s\n" input_filename;
  Printf.printf "Output Filename: %s\n" output_filename;
  Printf.printf "Debug: %b\n" debug_msgs;
  let file_contents = MyUtil.read_file input_filename in
  let toks = MyLexing.tokenize file_contents 0 in
  (*List.iter (fun tok -> print_endline (Token.string_of_token tok)) toks;*)
  let lex_file = MyParsing.parse toks in 
  let cases = lex_file.rule.cases in 
  let rec aux (cases : MyParsing.case list)= 
    match cases with 
    | [] -> ()
    | x :: xs -> 
      let flat, _, _ = MyParsing.postorder x.regex in 
      Printf.printf "Regex: %s\n  -> " (MyParsing.string_of_regex x.regex); 
      let strli = List.map MyParsing.string_of_flat_regex flat in
      List.iter print_string strli;
      print_endline "";
      aux xs;
  in
  aux cases;
  let postorder_tuples = 
    List.map (fun (case : MyParsing.case) -> MyParsing.postorder case.regex) cases 
  in
  let num_tuples = List.length postorder_tuples in
  let number_strings = 
    List.fold_right (fun (_,_,nStr) acc -> nStr + acc) postorder_tuples 0
  in 
  let postorder_tuples = 
    List.map (fun (re,len_re,_) -> (re,len_re)) postorder_tuples
  in
  let ttable, ctable, start, dead, size = _make_dfa postorder_tuples num_tuples number_strings in
  Printf.printf "Start State: %d\n" start;
  Printf.printf "Dead State: %d\n" dead;
  Printf.printf "Number of States: %d\n" size;
  Printf.printf "ctable size: %d\n" (Array.length ctable);
  Printf.printf "ttable size: %d x %d\n" (Array.length ttable) (Array.length ttable.(0));
  let gen_context = CppTemplate.context output_filename in
  let src_context = CodeGen.create_source_context start dead size ttable ctable lex_file in
  CodeGen.generate_code src_context gen_context;
  ()