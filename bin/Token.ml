(* 
file  : token.ml
brief : provides the type 
*)

module Token = struct
  type token =
  | RULE
  | ID of string 
  | EQUALS
  | BAR
  | CODE of string
  | STRING of string
  | REGEX of string
  | ALIAS
  | EOF

  let index_of_token (tok : token) : int =
    match tok with
    | RULE -> 0
    | ID(_) -> 1
    | EQUALS -> 2
    | BAR -> 3
    | CODE(_) -> 4
    | STRING(_) -> 5
    | REGEX(_) -> 6
    | ALIAS -> 7
    | EOF -> 8

  let make_token ( index : int ) ( str : string) : token =
    match index with
      0 -> RULE
    | 1 -> ID ( str )
    | 2 -> EQUALS
    | 3 -> BAR
    | 4 -> CODE ( str  )
    | 5 -> STRING ( str )
    | 6 -> REGEX ( str )
    | 7 -> ALIAS
    | 8 -> EOF
    | _ -> failwith "Invalid index"

  let string_of_token ( tok : token ) : string = 
    match tok with
      RULE -> "RULE"
    | ID(id) -> "ID( " ^ id ^ ")"
    | EQUALS -> "EQUALS"
    | BAR -> "BAR"
    | EOF -> "EOF"
    | CODE(code) -> "CODE ( " ^ code ^ ")"
    | _ -> failwith ("Unimplemented: " ^ (string_of_int (index_of_token tok)))

end