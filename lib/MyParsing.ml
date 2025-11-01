(*
  MyLexing.ml
  --------------
  Small helper module used by the parser driver in `bin/main.ml`.
*)

module MyParsing = struct
  open Token
  open MyUtil
  open Regex
  
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
    regex : Regex.ast
  }
    
  let parse (toks : Token.token list) = 
    let lex_file = ref {header="";trailer="";rule={name="";return_type="";none_code="";eof_code="";cases=[]}} in
    let case_list = ref [] in
    let symtable = Hashtbl.create 10 in
    
    (* [parse_assignments toks] parses the assignments before the rule 
        declare. Assignments must be of the form <id> = <regex>
        @param toks token list to parse
        @return remaining toks after parse *)
    let rec parse_assignments toks = 
      match toks with 
      | Token.LET :: Token.ID(id) :: Token.EQUALS :: toks ->
        let re, rest = parse_regex toks in
        Hashtbl.add symtable id re;
        parse_assignments rest
      | _ -> toks

    (* [parse_rule toks] parses the rule itself. Modifies lex file obj
        via ref inline.
        @param toks token list to parse
        @return remaining toks after parse *)
    and parse_rule toks = 
      match toks with 
      | Token.RULE :: Token.ID(name) :: Token.COLON :: Token.ID(return_type) :: Token.EQUALS :: Token.PARSE :: rest -> 
        let new_rule = {!lex_file.rule with name = name; return_type = return_type;} in 
        lex_file := { !lex_file with rule = new_rule };
        parse_cases rest 
      | _ -> failwith "rule declarations must be in the form rule <id> : <return> = parse"
    and parse_cases (toks : Token.token list) =
      let case, toks = parse_case toks in 
      (
        match case.regex with 
        | Emptyset -> lex_file := {!lex_file with rule={!lex_file.rule with none_code=case.code}}
        | Eof -> lex_file := {!lex_file with rule={!lex_file.rule with eof_code=case.code}}
        | _ -> case_list := (case :: !case_list) 
      );
      match toks with 
      | ( Token.BAR :: _  as rest ) -> parse_cases rest 
      | _ -> toks 
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
        | tok :: _ -> fmt_failwith "parse_case : Unexpected token : %s" (Token.string_of_token tok)
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
      | ( (Token.STRING _ | CHAR _ | STAR | LPAREN | LBRACKET | ID _ ) :: _ as rest ) ->
        let right, rest = parse_repeat rest in
        Concat (left,right), rest
      | _ -> left, toks 
    and parse_repeat (toks : Token.token list) = 
      let atom, toks = parse_atomic toks in
      match toks with 
      | Token.STAR :: rest -> Star atom, rest
      | Token.PLUS :: rest -> Concat (atom , (Star atom)), rest
      | _ -> atom, toks
    and parse_atomic (toks : Token.token list) = 
      match toks with 
      | Token.NONE_PATT :: rest -> Emptyset, rest
      | Token.EOF_PATT :: rest -> Eof, rest
      | Token.STRING(str) :: rest -> Literal str, rest
      | Token.CHAR(c) :: rest -> Char c, rest
      | Token.ID(id) :: rest ->
      (
        match Hashtbl.find_opt symtable id with 
        | Some re -> re,rest
        | None -> MyUtil.fmt_failwith "Undeclared identifier \"%s\"" id
      )
      | Token.LBRACKET :: toks -> 
      (
        let inv = (match toks with Token.CAROT :: _ -> true | _ -> false) in
        let items, rest = parse_range_items toks inv in
        match rest with 
        | Token.RBRACKET :: rest -> items, rest
        | _ -> failwith "Unmatched '['"
      )
      | Token.LPAREN :: toks ->
      (
        let re, rest = parse_regex toks in
        match rest with 
        | Token.RPAREN :: rest -> re, rest
        | _ -> failwith "Unmatched '('"
      )
      | tok :: _ -> fmt_failwith "Unexpected token : %s, expected atomic." (Token.string_of_token tok)
      | _ -> failwith "Missing atomic"
    and parse_range_items (toks : Token.token list) (inv : bool)=
      let rec aux range toks =  
        let left, toks = parse_range_item toks in 
        match toks with
        | (Token.RBRACKET :: _ as rest) -> (Regex.Charset (left::range, inv)), rest
        | _ -> aux (left :: range) toks
      in aux [] toks 
    and parse_range_item (toks : Token.token list) = 
      match toks with
      | Token.CHAR(left) :: Token.DASH :: Token.CHAR(right) :: rest -> 
        (left,right), rest 
      | Token.CHAR(c) :: rest -> (c,c), rest 
      | tok :: _ -> fmt_failwith "Expected character token, not %s" (Token.string_of_token tok)
      | [] -> failwith "Range must not be empty"
    in 
    (* possibly extract header code *)
    let toks = 
      match toks with
      | Token.CODE(header) :: rest -> lex_file := {!lex_file with header=header}; rest
      | _ -> toks 
    in 
    (* parse the assignments and rule *)
    let toks = parse_assignments toks in 
    let toks = parse_rule toks in
    
    (* possibly extract the trailer code, and enforce no extra symbols *)
    (
      match toks with 
      | Token.CODE(trailer) :: rest ->
      ( 
        lex_file := {!lex_file with trailer=trailer};
        match rest with 
        | Token.EOF :: [] -> ()
        | tok :: _ -> fmt_failwith "Unexpected symbol after trailer : %s" 
          (Token.string_of_token tok)
        | [] -> failwith "expected eof, got []"
      )
      | Token.EOF :: rest -> 
      (
        match rest with
        | [] -> ()
        | tok :: _ -> fmt_failwith "Unexpected symbol after rule block : %s" 
          (Token.string_of_token tok)
      )
      | tok :: _ -> fmt_failwith "Expected trailer or eof, not %s" (Token.string_of_token tok)
      | [] -> failwith "expected eof, got []"
    )
    ;

    let new_rule = {!lex_file.rule with cases = List.rev !case_list} in 
    {!lex_file with rule=new_rule} 

end