(*
  MyLexing.ml
  --------------
  Small helper module used by the parser driver in `bin/main.ml`.
*)

module MyParsing = struct
  open Token
  open MyUtil

  let fmt_failwith = MyUtil.fmt_failwith

  type lex_file = {
    header : string;
    trailer : string;
    rule : rule;
  }
  and rule = {
    name : string; 
    return_type : string;
    none_code : string;
    eof_code : string;
    cases : case list
  }
  and case = {
    alias : string;
    code : string;
    regex : regex;
  }
  and regex = 
  | Emptyset 
  | Eof 
  | Char of char
  | String of string
  | Union of regex * regex
  | Cat of regex * regex
  | Star of regex 
  | CharRange of char * char

  type flat_regex = 
  | FChar of char
  | FString of string
  | FCharRange of char * char 
  | FUnion
  | FCat
  | FStar

  let string_of_flat_regex (fr : flat_regex) =
    match fr with
    | FChar c -> Printf.sprintf "'%s'" (MyUtil.descape c)
    | FString s ->  Printf.sprintf "\"%s\"" s
    | FCharRange (c1,c2) -> Printf.sprintf "['%s'-'%s']" (MyUtil.descape c1) (MyUtil.descape c2)
    | FUnion -> "|" 
    | FCat -> "·" 
    | FStar -> "*"

  let rec string_of_regex (r : regex) = 
    match r with
    | Emptyset -> "∅"
    | Eof -> "$"
    | Char c -> Printf.sprintf "'%s'" (MyUtil.descape c)
    | String s -> Printf.sprintf "\"%s\"" s
    | Union (r1,r2) -> Printf.sprintf "(%s | %s)" (string_of_regex r1) (string_of_regex r2)
    | Cat (r1,r2) -> Printf.sprintf "(%s · %s)" (string_of_regex r1) (string_of_regex r2)
    | Star r -> Printf.sprintf "(%s *)" (string_of_regex r)
    | CharRange (c1,c2) -> Printf.sprintf "['%s'-'%s']" (MyUtil.descape c1) (MyUtil.descape c2)

  (** [postorder ast_root] returns the postorder flat_re typed list of [ast_root] along with
      the length of this list, and the number of string types in this list.
      @param ast_root the root of the regex ast 
      @return postorder list, size, nStrings *)
  let postorder (ast_root : regex) : flat_regex list * int * int = 
    let rec aux todo result len numStr = 
      match todo with
      | [] -> result, len, numStr
      | Char c :: todo -> aux todo ((FChar c)::result) (len+1) numStr
      | String s :: todo -> aux todo ((FString s)::result) (len+1) (numStr)
      | CharRange (lo,hi) :: todo -> aux todo (FCharRange (lo,hi)::result) (len+1) numStr
      | Union (left, right) :: todo -> aux (right::left::todo) (FUnion::result) (len+1) numStr
      | Cat (left,right) :: todo -> aux (right::left::todo) (FCat::result) (len+1) numStr
      | Star regex :: todo -> aux (regex::todo) (FStar::result) (len+1) numStr
      | regex :: _ -> fmt_failwith "Regex %s should not be used in postoder" (string_of_regex regex)
    in
    aux [ast_root] [] 0 0
    
  let parse (toks : Token.token list) = 
    let lex_file = ref {header="";trailer="";rule={name="";return_type="";none_code="";eof_code="";cases=[]}} in
    let case_list = ref [] in
    let rec parse_rule toks = 
      match toks with 
      | Token.RULE :: Token.ID(name) :: Token.COLON :: Token.ID(return_type) :: Token.EQUALS :: Token.PARSE :: rest -> 
        let new_rule = {!lex_file.rule with name = name; return_type = return_type;} in 
        let _ = lex_file := { !lex_file with rule = new_rule } in
        parse_cases rest 
      | _ -> failwith "rule declarations must be in the form rule <id> : <return> = parse"
    and parse_cases (toks : Token.token list) =
      let case, toks = parse_case toks in 
      let _ = 
        match case.regex with 
        | Emptyset -> lex_file := {!lex_file with rule={!lex_file.rule with none_code=case.code}}
        | Eof -> lex_file := {!lex_file with rule={!lex_file.rule with eof_code=case.code}}
        | _ -> case_list := (case :: !case_list) 
      in
      match toks with 
      | ( Token.BAR :: _  as rest ) -> parse_cases rest 
      | Token.CODE(trailer) :: Token.EOF :: [] -> lex_file := {!lex_file with trailer =trailer;}
      | Token.CODE(_) :: tok :: _ | tok :: _ -> fmt_failwith "Unexpected token : %s" (Token.string_of_token tok)
      | [] -> () (* there is no trailer code *)
    and parse_case (toks : Token.token list) =
      let mustbar, toks = MyUtil.head toks in 
      if (match mustbar with Token.BAR -> false | _ -> true) then 
        fmt_failwith "Expected '|' not token : %s" (Token.string_of_token mustbar)
      else
        let regex, toks = parse_regex toks in 
        let case = {alias="";code="";regex=regex;} in
        match toks with
        | Token.AS :: Token.ID(id) :: Token.CODE(code) :: rest -> {case with alias=id;code=code;}, rest
        | Token.CODE(code) :: rest -> {case with code=code;}, rest
        | tok :: _ -> fmt_failwith "Unexpected token : %s" (Token.string_of_token tok)
        | [] -> fmt_failwith "Expected atleast 1 case in rule \"%s\"" !lex_file.rule.name
    and parse_regex (toks : Token.token list) = parse_union toks
    and parse_union (toks : Token.token list) =
      let left, toks = parse_cat toks in 
      match toks with 
      | Token.BAR :: rest -> 
        let right, rest = parse_union rest in
        Union (left,right), rest 
      | toks -> left, toks
    and parse_cat (toks : Token.token list) = 
      let left, toks = parse_repeat toks in
      match toks with
      | ( (Token.BAR | STRING _ | REGEX _ | CHAR _ | STAR | LPAREN | RPAREN | LBRACKET | DASH | RBRACKET ) :: _ as rest ) ->
        let right, rest = parse_cat rest in
        Cat (left,right), rest
      | _ -> left, toks 
    and parse_repeat (toks : Token.token list) = 
      let atom, toks = parse_atomic toks in
      match toks with 
      | Token.STAR :: rest -> Star atom, rest
      | _ -> atom, toks
    and parse_atomic (toks : Token.token list) = 
      match toks with 
      | Token.NONE_PATT :: rest -> Emptyset, rest
      | Token.EOF_PATT :: rest -> Eof, rest
      | Token.STRING(str) :: rest -> String str, rest
      | Token.CHAR(c) :: rest -> Char c, rest
      | Token.LBRACKET :: toks -> 
      (
        let items, rest = parse_range_items toks in
        match rest with 
        | Token.RBRACKET :: rest -> items, rest
        | _ -> failwith "Unmatched '['"
      )
      | tok :: _ -> fmt_failwith "Unexpected token : %s, expected atomic." (Token.string_of_token tok)
      | _ -> failwith "Missing atomic"
    and parse_range_items (toks : Token.token list) = 
      let left, toks = parse_range_item toks in 
      match toks with
      | (Token.RBRACKET :: _ as rest) -> left, rest
      | _ -> 
        let right,rest = parse_range_items toks in
        Union (left, right), rest
    and parse_range_item (toks : Token.token list) = 
      match toks with
      | Token.CHAR(left) :: Token.DASH :: Token.CHAR(right) :: rest -> 
        CharRange (left,right), rest 
      | Token.CHAR(c) :: rest -> Char c, rest 
      | tok :: _ -> fmt_failwith "Expected character token, not %s" (Token.string_of_token tok)
      | [] -> failwith "Range must not be empty"
    in 
    let _ = 
      match toks with
      | Token.CODE(header) :: toks -> let _ = lex_file := {!lex_file with header=header} in parse_rule toks
      | toks -> parse_rule toks 
    in 
    let new_rule = {!lex_file.rule with cases = List.rev !case_list} in 
    {!lex_file with rule=new_rule} 

end