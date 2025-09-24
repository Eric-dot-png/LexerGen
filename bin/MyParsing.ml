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
    args : string list;
    cases : case list;
  }
  and case = {
    pattern : pattern;
    alias : string;
    code : string
  }
  and pattern =
  | String of string
  | Regex of string
  | None
  | Eof

  let print_lex_file ( file : lex_file ) : unit = 
    let line = String.make 50 '*' in
    let _ = print_endline line in 
    let _ = print_endline file.header in 
    let f_rule = file.rule in 
    let _ = Printf.printf "rule %s = parse\n" f_rule.name in

    let string_of_pattern ( patt : pattern ) : string =
    ( 
      match patt with 
      | None -> "_" 
      | Eof -> "eof"
      | String(str) -> str
      | Regex(rgx) ->  rgx 
    ) 
    in

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
      | Token.CODE(code)::toks -> (get_rule toks {ret with header=code}) 
      | _ -> ( get_rule toks {ret with header=""} )

    and get_rule (toks : Token.token list ) ( ret : lex_file ): lex_file = 
      match toks with 
      | Token.RULE :: Token.ID(id) :: Token.EQUALS :: Token.PARSE::toks ->
        get_cases toks { ret with rule={name=id;args=[];cases=[];}}
      | _ -> failwith "Error" 
    
    and get_cases (toks : Token.token list) ( ret : lex_file ) : lex_file = 
      match toks with
      | Token.BAR :: ( ( Token.REGEX(_) | Token.STRING(_) | Token.NONE_PATT | Token.EOF_PATT) :: _ as toks )  -> get_case toks ret 
      | Token.BAR :: _ -> failwith "Expected pattern following '|' "
      | _ -> failwith "Expected '|' "

    and get_case (toks : Token.token list) ( ret : lex_file ) : lex_file =
      let patt_tok, toks = MyUtil.head toks in
      let determine_patt tok = 
      (  
        match tok with
        | Token.NONE_PATT -> None
        | Token.EOF_PATT -> Eof 
        | Token.REGEX(patt)  -> Regex(patt)
        | Token.STRING(patt) -> String(patt)
        | _  -> failwith "Not possible"
      ) in
      let pattern_node = determine_patt patt_tok in 
      match toks with 
      | Token.AS :: Token.ID(id) :: Token.CODE(code) :: toks ->
      (
        match patt_tok with 
        | Token.NONE_PATT | Token.EOF_PATT -> failwith "None pattern / eof pattern not aliasable"
        | Token.STRING(_) | Token.REGEX(_) -> 
        (
          let case_i = { pattern = pattern_node; alias=id; code=code;} in
          let rule = { ret.rule with cases =  case_i :: ret.rule.cases } in
          let ret = { ret with rule=rule } in
          check_done toks ret 
        )
        | _ -> failwith "Not possible"
      )
      | Token.CODE(code) :: toks  -> 
      (
        let case_i = { pattern = pattern_node; alias=""; code=code;} in
        let rule = { ret.rule with cases =  case_i :: ret.rule.cases } in
        let ret = { ret with rule=rule } in
        check_done toks ret 
      )
      | _ -> failwith "Error"
    
    and check_done ( toks : Token.token list ) ( ret : lex_file ) : lex_file =
      match toks with 
      | ( Token.CODE(_) :: _ as toks ) -> get_trailer toks ret 
      | ( Token.BAR :: _ as toks ) -> get_cases toks ret 
      | _ -> failwith "Error"  

    and get_trailer ( toks : Token.token list ) ( ret : lex_file ) : lex_file = 
      match toks with
      | Token.CODE(code) :: Token.EOF :: [] -> {ret with trailer=code }
      | Token.CODE(_) :: _ -> failwith "Unexpected tokens after trailer"
      | _ -> failwith "Not Possible"
    in
    
    let lfile = (get_header toks {header=""; rule={name=""; args=[]; cases=[]}; trailer=""}) in
    let lrule = lfile.rule in
    let lrule = {lrule with cases = (List.rev lrule.cases)} in
    {lfile with rule=lrule}

end