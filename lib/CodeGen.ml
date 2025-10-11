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
    function type to generate case tag table source code
    - parameters:
      - [ctable] case tag table 
    - returns: string representation case tag table source code
  *)
  type ctable_gen_fn = ctable -> string 

  (**
    function type to generate action function source code for a single case
    - parameters
      - [case] the case to generate the action code for
      - [case_index] the index of `case` in the case list
    - returns string representation of the action function source code
  *)
  type action_gen_fn =  int -> MyParsing.case -> string 

  (**
    function type to generate the final source code wrapping the table and action code
    - parameters:
      - [ttable_str] string representation of the transition table
      - [ctable_str] list of string representations of the action code for each case
    - returns: final source code as a string
  *)
  type wrapper_gen_fn = int -> int -> string -> string -> string list -> string -> string -> string

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
    gen_function : source_context -> string
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
    MyUtil.write_file gen_ctx.out_file (gen_ctx.gen_function src_ctx)
   
end