(*
  CodeGen.ml
  --------
  Definitions for code generation module

  The module exposes:
  - function aliases for required functions to represent transition and case tables
  - types to represent context information required for code generation
  - functions to generate source code from a lexing rule
*)

module CodeGen = struct
  (*----------------------------------------------------------------------------*)
  (* Module Initialization                                                      *)
  (*----------------------------------------------------------------------------*)
  
  open MyParsing
  open MyUtil

  (*----------------------------------------------------------------------------*)
  (* Module Type Definitions                                                    *)
  (*----------------------------------------------------------------------------*)
  
  (**
    structure to represent the transition table
    - each array index corresponds to a state
    - each array value is an array of next states for each character in the alphabet
  *)
  type ttable = int array array

  (**
    stucture to represent the case (action) table
    - each array index corresponds to a state
    - the value of each index is the index of the case to execute for that state
  *)
  type ctable = int array 
  
  (**
    function type to generate transition table source code
    - parameters:
      - [ttable] the transition table to generate source code for
    - returns: string representation of the transition table
  *)
  type table_gen_fn = ttable -> string

  (**
    function type to generate action code for each case
    - parameters:
      - [case] the case to generate action code for
    - returns: string representation of the action code
  *)
  type action_gen_fn = MyParsing.case -> string 

  (**
    function type to generate the final source code wrapping the table and action code
    - parameters:
      - [table_str] string representation of the transition table
      - [action_strs] list of string representations of the action code for each case
    - returns: final source code as a string
  *)
  type wrapper_gen_fn = string -> string list -> string

  (**
    structure to represent context information required of source being generated
    - [start] the starting state of the lexer
    - [dead] the dead state of the lexer
    - [ttable] the transition table of the lexer
    - [ctable] the case (action) table of the lexer
  *)
  type source_context = {
    start : int;
    dead : int;
    num_states : int;
    ttable : ttable;
    ctable : ctable;
    lex_file : MyParsing.lex_file;
  }

  (**
    structure to represent context information required to generate source code
    - [out_file] path to output file
    - [gen_table] function to generate transition table source code
    - [gen_action] function to generate action code for each case
    - [gen_wrapper] function to generate the final source code wrapping the table and action code
  *)
  type gen_context = {
    out_file : string;
    gen_table : table_gen_fn;
    gen_action : action_gen_fn;
    gen_wrapper : wrapper_gen_fn;
  }

  (*----------------------------------------------------------------------------*)
  (* Module function definitions                                                *)
  (*----------------------------------------------------------------------------*)
  
  let create_source_context ( start : int ) ( dead : int ) ( num_states : int )
    ( ttable : ttable ) ( ctable : ctable ) ( lex_file : MyParsing.lex_file ) 
    : source_context = 
    {
      start = start;
      dead = dead;
      num_states = num_states;
      ttable = ttable;
      ctable = ctable;
      lex_file = lex_file;
    }
    
  (**
    generate a source context from the given parameters
    - [src_ctxt] context of the source to generate from
    - [gen_ctxt] context of the generation process
  *)
  let generate_code (src_ctx : source_context) (gen_ctx : gen_context) : unit =
    let ttable_str = gen_ctx.gen_table src_ctx.ttable in
    let action_strs = List.map gen_ctx.gen_action src_ctx.lex_file.rule.cases in
    let source_code = gen_ctx.gen_wrapper ttable_str action_strs in
    let _ = print_string source_code in 
    MyUtil.write_file gen_ctx.out_file source_code
   
end