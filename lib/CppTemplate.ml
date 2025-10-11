(*
  CppTemplate.ml
  --------------
  Definitions for C++ code generation template

  This module exposes:
  - functions to generate C++ source code from a lexing rule

*)

module CppTemplate = struct
  (*----------------------------------------------------------------------------*)
  (* Module Initialization                                                      *)
  (*----------------------------------------------------------------------------*)
  
  open CodeGen
  open MyParsing 

  let table_gen ( ttable : CodeGen.ttable ) : string =
    let _ = Printf.printf "Generating transition table with %d states\n" (Array.length ttable) in
    let row_to_str ( row : int array ) : string =
      let elems = Array.map string_of_int row in
      let inner = String.concat ", " ( Array.to_list elems ) in
      "{" ^ inner ^ "}" in
    let rows = Array.map row_to_str ttable in
    let body = String.concat ",\n  " ( Array.to_list rows ) in
    Printf.sprintf "std::array<std::array<int, %d>, %d> __ttable = {\n %s \n};\n" 
      (Array.length ttable.(0)) (Array.length ttable) body

  let action_gen ( case : MyParsing.case ) : string = 
    let alias_str = if String.length case.alias > 0 then 
      Printf.sprintf "auto %s = yytext;\n" case.alias else "" in
    Printf.sprintf "{\n%s%s\n}" alias_str case.code

  let wrapper_gen ( table_str : string ) ( action_strs : string list ) : string =
    let actions_body = String.concat "\n" action_strs in
    Printf.sprintf {|
#include <array>

%s

%s

    |} table_str actions_body


  let context ( out_file : string ) : CodeGen.gen_context = 
    {
      out_file = out_file;
      gen_table = table_gen;
      gen_action = action_gen;
      gen_wrapper = wrapper_gen;
    }

end