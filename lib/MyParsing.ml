(*
  MyLexing.ml
  --------------
  Small helper module used by the parser driver in `bin/main.ml`.
*)

module MyParsing = struct
  open Token
  open MyUtil

  type lex_file = {
    header : string;
    rule : rule;
    trailer : string;
  }
  and rule = {
    name : string;
    return_type : string;
    cases : case list;
    none_code : string;
    eof_code : string;
  }
  and case = {
    pattern : pattern;
    alias : string;
    code : string
  }
  and pattern =
  | Regex of string
  | String of string

  let clean_string_of_pattern ( patt : pattern ) : string = 
    match patt with String(s) | Regex(s) -> MyUtil.trim s

  let pattern_index ( patt : pattern ) : int = 
    match patt with
    | Regex(_) -> 0
    | String(_) -> 1

  let string_of_pattern ( patt : pattern ) : string =
      match patt with 
      | String(str) -> str
      | Regex(rgx) ->  rgx 

  let print_lex_file ( file : lex_file ) : unit = 
    let line = String.make 50 '*' in
    let _ = print_endline line in 
    let _ = print_endline file.header in 
    let f_rule = file.rule in 
    let _ = Printf.printf "rule %s = parse\n" f_rule.name in

    let rec print_cases ( cases : case list ) : unit = 
      match cases with 
      | case::cases -> 
      (
        let patt_str = string_of_pattern case.pattern in
        if String.length case.alias != 0 then
          let _ = Printf.printf "| %s as %s %s\n" patt_str case.alias case.code in 
          print_cases cases 
        else 
          let _ = Printf.printf "| %s %s\n" patt_str case.code in
          print_cases cases
      )
      | [] -> ()
    in 
    let _ = print_cases f_rule.cases in
    let _ = print_endline file.trailer in
    print_endline line

  let parse ( toks : Token.token list ) : lex_file =
    let rec get_header ( toks : Token.token list ) ( ret : lex_file ): lex_file = 
      match toks with
      | Token.CODE(code)::toks -> (get_rule toks {ret with header=MyUtil.trim code}) 
      | _ -> ( get_rule toks {ret with header=""} )

    and get_rule (toks : Token.token list ) ( ret : lex_file ): lex_file = 
      match toks with 
      | Token.RULE :: Token.ID(id) :: Token.COLON :: Token.ID(ret_type) :: Token.EQUALS :: Token.PARSE::toks ->
        get_cases toks { ret with rule={name=id;return_type=ret_type;cases=[];none_code="";eof_code="";}}
      | _ -> failwith "Error" 
    
    and get_cases (toks : Token.token list) ( ret : lex_file ) : lex_file = 
      match toks with
      | Token.BAR :: ( ( Token.REGEX(_) | Token.STRING(_) | Token.NONE_PATT | Token.EOF_PATT) :: _ as toks )  -> get_case toks ret 
      | Token.BAR :: _ -> failwith "Expected pattern following '|' "
      | _ -> failwith "Expected '|' "

    and get_case (toks : Token.token list) ( ret : lex_file ) : lex_file =
      let helper tok alias code = 
        match tok with 
        | Token.REGEX(patt) -> {pattern=Regex(patt);alias=alias;code=code}
        | Token.STRING(patt) -> {pattern=String(patt);alias=alias;code=code}
        | _ -> failwith "Not possible"
      in
      match toks with
      (* correct special cases (none and eof) *)
      
      | Token.NONE_PATT :: Token.CODE(none_code) :: rest -> 
        let ret_rule = {ret.rule with none_code = MyUtil.trim none_code} in
        check_done rest {ret with rule = ret_rule }
      
      | Token.EOF_PATT :: Token.CODE(eof_code) :: rest -> 
        let ret_rule = {ret.rule with eof_code = MyUtil.trim eof_code} in
        check_done rest {ret with rule = ret_rule }
      
      (* incorrect special cases *)
      | ( Token.EOF_PATT | Token.NONE_PATT ) :: Token.AS :: _ -> failwith "Special Patterns (none and eof) are not aliasable" 
      
      (* correct non-special cases *)
      | ( Token.REGEX(_) | Token.STRING(_) as patt_tok ) :: Token.AS :: Token.ID(alias) :: Token.CODE(code) :: rest ->
        let case_i = helper patt_tok alias (MyUtil.trim code) in 
        let rule = { ret.rule with cases =  case_i :: ret.rule.cases } in
        check_done rest { ret with rule=rule }
      
      | ( Token.REGEX(_) | Token.STRING(_) as patt_tok ) :: Token.CODE(code) :: rest -> 
        let case_i = helper patt_tok "" (MyUtil.trim code) in 
        let rule = { ret.rule with cases =  case_i :: ret.rule.cases } in
        check_done rest { ret with rule=rule }
      
        (* incorrect non-special cases *)
      | (Token.REGEX(_) | Token.STRING(_) ) :: Token.AS :: Token.CODE(_) :: _ -> failwith "Missing identifier for alias"
      
      | _ -> failwith "Unexpected token"

    and check_done ( toks : Token.token list ) ( ret : lex_file ) : lex_file =
      match toks with 
      | ( Token.CODE(_) :: _ as toks ) -> get_trailer toks ret 
      | ( Token.BAR :: _ as toks ) -> get_cases toks ret 
      | _ -> failwith "Error"  

    and get_trailer ( toks : Token.token list ) ( ret : lex_file ) : lex_file = 
      match toks with
      | Token.CODE(code) :: Token.EOF :: [] -> {ret with trailer=MyUtil.trim code}
      | Token.CODE(_) :: _ -> failwith "Unexpected tokens after trailer"
      | _ -> failwith "Not Possible"
    in
    
    let lfile = (get_header toks {header=""; rule={name=""; return_type=""; cases=[];none_code="";eof_code=""}; trailer=""}) in
    let lrule = lfile.rule in
    let lrule = {lrule with cases = (List.rev lrule.cases)} in
    {lfile with rule=lrule}

    let flatten_rule ( rule : rule ) : (string * int * string * string) list =
      let flatten_case ( case : case ) : string * int * string * string =
         ((clean_string_of_pattern case.pattern),  (pattern_index case.pattern), case.alias, case.code) in
      List.map flatten_case rule.cases

end