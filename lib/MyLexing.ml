(*
  MyLexing.ml
  --------------
  Small helper module used by the lexer driver in `bin/main.ml`.

  Responsibilities:
  - Provide a lightweight lexing buffer type `lexbuf` (a small cursor-based
    view over an input string).
  - Implement very small scanning routines used to identify tokens produced
    by the meta-lexer (this file intentionally keeps runtime behaviour
    minimal — the real token definitions live in `Token`).

*)

module MyLexing = struct
  open Token
  open MyUtil

  let fmt_failwith = MyUtil.fmt_failwith

  let tokenize (str : string) = 
    let len    = String.length str in
    let pos    = ref 0 in 
    let line   = ref 0 in
    let column = ref 0 in 

    let next() = if !pos < len then Some str.[!pos] else None in 
    let peek() = if (!pos + 1) < len then Some str.[!pos+1] else None in
    
    let newline() = line := !line + 1; column := 0 in

    let adv() = pos := !pos + 1; column := !column + 1 in  

    let match_while (continue : char option -> bool) =
      let start = !pos in 
      let _ = adv () in 
      let _ = while continue ( next () ) do
        adv ()
      done in
      let matched = match next () with None -> false | _ -> true in
      (String.sub str start (!pos-start+1)), matched
    in

    let rec lex () = 
      match next() with  
      | None | Some '\x00' -> Token.EOF
      | Some (' ' | '\t' | '\r' ) -> adv (); lex ()
      | Some '\n' -> adv(); newline(); lex ()
      | Some ':' -> Token.COLON
      | Some '|' -> Token.BAR
      | Some '*' -> Token.STAR
      | Some '(' -> Token.LPAREN
      | Some ')' -> Token.RPAREN
      | Some '[' -> Token.LBRACKET
      | Some '-' -> Token.DASH
      | Some ']' -> Token.RBRACKET
      | Some '=' -> Token.EQUALS
      | Some '+' -> Token.PLUS
      | Some '^' -> Token.CAROT
      | Some '_' when not (MyUtil.is_alnum_ex (peek())) -> Token.NONE_PATT
      | Some '\'' ->
      (
        let continue = fun c -> 
          match c with 
          | Some '\'' -> false
          | None -> false
          | _ -> true
        in let matched, found = match_while continue in
        match found, String.length matched with
        | false,_ -> fmt_failwith "Unmatched ''' in '%s" matched
        | _, 4 when matched.[1] = '\\' -> Token.CHAR (MyUtil.escape matched.[2])
        | _, 3 -> Token.CHAR (matched.[1])
        | _ -> fmt_failwith "'%s' is not a character." matched
      )
      | Some '{' -> 
      (
        let ident = ref 0 in 
        let continue = fun c -> 
          match c with 
          | Some '}' when !ident = 0 -> false
          | Some '}' when !ident > 0 -> ident := !ident-1; true
          | Some '{' -> ident := !ident+1; true          
          | None -> false
          | _ -> true
        in let matched,found = match_while continue in 
        if found then Token.CODE(MyUtil.trim matched) 
        else fmt_failwith "Unmatched '{' in %s" matched
      )
     | Some '"' -> 
      ( 
        let continue = fun c -> 
          match c with 
          | Some '"' -> false  
          | None -> false
          | _ -> true
        in let matched, found = match_while continue in 
        if not found then fmt_failwith "Unmatched '\"' in %s" matched
        else Token.STRING(MyUtil.trim matched)
      )
     | Some ('a'..'z' | 'A'..'Z' | '_' ) ->
      (
        let continue = fun c ->
          match c with 
          | Some ('a'..'z' | 'A'..'Z' | '_' | '0'..'9') -> true
          | _ -> let _ = pos := !pos - 1 in false
        in
        let matched, _ = match_while continue in
        match matched with
        | "rule" -> Token.RULE
        | "parse" -> Token.PARSE
        | "as" -> Token.AS
        | "eof" -> Token.EOF_PATT
        | "let" -> Token.LET
        | _ -> Token.ID(matched)
      )
     | Some c -> fmt_failwith "Unexpected character '%c'" c
    in
    let rec aux toks = 
      let tok = lex () in
      adv ();
      match tok with 
      | Token.EOF -> List.rev (tok :: toks)
      | _ -> aux (tok :: toks)
    in
    aux []
end