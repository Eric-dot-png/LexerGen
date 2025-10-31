(*
   Regex.ml
   --------------
   Small module containing regex types (ast, flat) And additional Utilities.
*)

module Regex = struct
  open MyUtil
  
  (** [ast] is the type to represent regular expressions as an
      abstract syntax tree. This type is used for better error handling, 
      and recursive descent parsing, but is ultimatly converted into
      the [flat] variation in a postorder form. *)
  type ast = 
  | Emptyset
  | Eof 
  | Char of char
  | Literal of string
  | Union of ast * ast
  | Concat of ast * ast
  | Star of ast 
  | Charset of char * char
  (* 
  Charset of (char * char) list * bool 

  ['a'-'z' '0'] -> charset of [('a','z'), ('0', '0') ]
  
  *)


  (** [string_of_ast r] returns the string representation of the in-order
      regular expression ast [r]
      @param root the root node of the regex-ast
      @returns the string representation in in-order form (parenthesized) *)  
  let rec string_of_ast (root : ast) = 
    match root with
    | Emptyset -> "∅"
    | Eof -> "$"
    | Char c -> Printf.sprintf "'%s'" (MyUtil.descape c)
    | Literal s -> Printf.sprintf "\"%s\"" s
    | Union (r1,r2) -> 
      Printf.sprintf "(%s | %s)" (string_of_ast r1) (string_of_ast r2)
    | Concat (r1,r2) -> 
      Printf.sprintf "(%s · %s)" (string_of_ast r1) (string_of_ast r2)
    | Star r -> Printf.sprintf "(%s *)" (string_of_ast r)
    | Charset (c1,c2) -> 
      Printf.sprintf "['%s'-'%s']" (MyUtil.descape c1) (MyUtil.descape c2)

  (** [flat] is the type to represent regular expressions in a non-tree manner.
      Specifically, this library uses them in postorder form for fast state 
      table construction. *)
  type flat = 
  | CharF of char
  | LiteralF of string
  | CharsetF of char * char 
  | UnionF
  | ConcatF
  | StarF

  (** [string_of_flat] returns the string representation of a singular flat
      regex symbol.
      @param re the single flat re symbol
      @return the string representation of re
  *)
  let string_of_flat (re : flat) =
    match re with
    | CharF c -> Printf.sprintf "'%s'" (MyUtil.descape c)
    | LiteralF s ->  Printf.sprintf "\"%s\"" s
    | CharsetF (c1,c2) -> 
      Printf.sprintf "['%s'-'%s']" (MyUtil.descape c1) (MyUtil.descape c2)
    | UnionF -> "|" 
    | ConcatF -> "·" 
    | StarF -> "*"

  (** [flat_postorder] is aliased to flat list to establish the "typeness"
      of a flattened, postorder, regular expression, which is used internally
      in cpp, as it's faster to pass and process. *)
  type flat_postorder = flat list

  (** [postorder_of_list ast_root] returns the post-order, flattened version 
      of the regular expression given by the ast_root.
      @param ast_root the root of the regex ast
      @returns 3-tuple of the flattened postorder regex, the size of the regex
               and the number of strings contained inside the ast. *)
  let flat_postorder_of_ast (ast_root : ast) : flat_postorder * int * int = 
    let rec aux (todo : ast list) result len numStr = 
      match todo with
      | [] -> result, len, numStr
      | Char c :: todo -> aux todo ((CharF c)::result) (len+1) numStr
      | Literal s :: todo -> aux todo ((LiteralF s)::result) (len+1) (numStr)
      | Charset (lo,hi) :: todo -> 
        aux todo (CharsetF (lo,hi)::result) (len+1) numStr
      | Union (left, right) :: todo -> 
        aux (right::left::todo) (UnionF::result) (len+1) numStr
      | Concat (left,right) :: todo -> 
        aux (right::left::todo) (ConcatF::result) (len+1) numStr
      | Star regex :: todo -> aux (regex::todo) (StarF::result) (len+1) numStr
      | regex :: _ -> 
        MyUtil.fmt_failwith "Regex %s should not be used in postoder" (string_of_ast regex)
    in
    aux [ast_root] [] 0 0

end