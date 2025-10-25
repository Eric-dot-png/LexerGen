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

  (* ------------------------------------------------------------------ *)
  (* Types and core data structures                                     *)
  (* ------------------------------------------------------------------ *)

  (** A tiny lexing buffer over an immutable input string.
      Fields:
      - [cstream]: the entire input string being scanned.
      - [stream_index]: current cursor position used for peeking/advancing.
      - [head_index]: start index of the current lexeme (inclusive).
      - [tail_index]: end index of the current lexeme (inclusive).
      - [last_rule]: numerical id of the last matched token/rule.
  *)
  type lexbuf = {
    cstream : string;
    stream_index : int;
    head_index : int;
    tail_index : int;
    last_rule : int;
  }

  (* ------------------------------------------------------------------ *)
  (* Constants / sentinel values                                        *)
  (* ------------------------------------------------------------------ *)

  (** EOF sentinel character (NUL). Using [char_of_int 0] keeps the type
      explicit and avoids embedding a literal '\000'. *)
  let eof = char_of_int 0

  (** Numeric constant representing a non-rule lexbuf *)
  let no_rule = -1
  
  (** Numeric constant representing a dead state *)
  let dead_state = -1

  (* ------------------------------------------------------------------ *)
  (* Construction / helpers                                             *)
  (* ------------------------------------------------------------------ *)

  (** Create an initial lexbuf over [cstream]. *)
  let lexbuf_of_string (cstream : string) : lexbuf =
    { cstream = cstream; stream_index = 0; head_index = 0; tail_index = 0; last_rule = no_rule }

  (** Return human readable string of [buf]*)
  let string_of_lexbuf ( buf : lexbuf ) : string =
    Printf.sprintf "<lexbuf cstream=\"%s\" stream_index=%d head_index=%d tail_index=%d last_rule=%d>"  
      buf.cstream buf.stream_index buf.head_index buf.tail_index buf.last_rule 
    

  (** Return the character under the cursor without advancing. If the
      cursor is past the end of the input, return [eof]. *)
  let peek (buf : lexbuf) : char =
    if buf.stream_index < String.length buf.cstream then
      buf.cstream.[buf.stream_index]
    else
      eof

  (** Extract the last matched lexeme between [head_index] (inclusive) and
      [tail_index] (exclusive). *)
  let matched_of_lexbuf (buf : lexbuf) : string =
    let start = buf.head_index in
    let len = buf.tail_index - buf.head_index in
    if len <= 0 then "" else String.sub buf.cstream start len

  (* ------------------------------------------------------------------ *)
  (* Tokenization pipeline (small state machine)                        *)
  (* ------------------------------------------------------------------ *)

  (** Scan the input starting at [buf.stream_index] and produce the next
      token together with an updated buffer. The function is intentionally
      small and imperative-style: it threads the [lexbuf] through helper
      states and never mutates shared state. *)
  let tokenize (buf : lexbuf) : Token.token * lexbuf =
    (* scan_state: starting scanning state that recognizes top-level tokens *)
    let rec scan_state (buf : lexbuf) : lexbuf =
      match peek buf with
      | ':' -> { buf with tail_index = buf.stream_index+1; last_rule = Token.index_of_token Token.COLON }
      | '|' -> { buf with tail_index = buf.stream_index+1; last_rule = Token.index_of_token Token.BAR }
      | '=' -> { buf with tail_index = buf.stream_index+1; last_rule = Token.index_of_token Token.EQUALS }      
      | '_' -> { buf with tail_index = buf.stream_index+1; last_rule = Token.index_of_token Token.NONE_PATT }
      | '{' -> code_state 0 { buf with stream_index = buf.stream_index + 1 }
      | '<' -> regex_state '<' { buf with stream_index = buf.stream_index+1 }
      | '\"' -> string_state '\"' { buf with stream_index = buf.stream_index+1 }
      | ' ' | '\n' | '\t' -> 
      (
        let new_index = buf.stream_index + 1 in
        let buf = {buf with stream_index = new_index; head_index = new_index } in
        scan_state buf 
      )
      | c when MyUtil.is_alpha c -> id_state { buf with stream_index = buf.stream_index + 1; last_rule = Token.index_of_token ( ID "" )}
      | c when c = eof ->
      (
        if 
          buf.last_rule = no_rule then { buf with last_rule = Token.index_of_token Token.EOF }
        else 
          buf
      )
      | c -> failwith (Printf.sprintf "Error: Character '%c' unexpected" c)

    (* code_state: scan a nested { ... } code block, tracking nesting level.
       When the outermost closing brace is found we record a CODE token. *)
    and code_state (indent : int) (buf : lexbuf) : lexbuf =
      match peek buf with
      | '{' -> code_state (indent + 1) { buf with stream_index = buf.stream_index + 1 }
      | '}' ->
      (
        if indent = 0 then
          { buf with tail_index = buf.stream_index+1; last_rule = Token.index_of_token (Token.CODE "") }
        else 
          code_state (indent - 1) { buf with stream_index = buf.stream_index + 1 }
      )
      | c when c = eof -> failwith "Unmatched '{'"
      | _ -> code_state indent { buf with stream_index = buf.stream_index + 1 }    
    
    (* id_state: scan as many alphanumeric characters and '_' to form an identifier. 
       Already consumed a letter. *)
    and id_state ( buf : lexbuf ) : lexbuf = 
      match peek buf with 
      | c when (MyUtil.is_alnum c) || c = '_' -> id_state {buf with stream_index = buf.stream_index+1}
      | _ -> { buf with tail_index = buf.stream_index; }

    (* regex_state: scan for a regular expression pattern. *)
    and regex_state (prev : char) ( buf : lexbuf ) : lexbuf =
      let curr = (peek buf) in
      match prev, curr with
      | '\\', '>' -> regex_state curr {buf with stream_index= buf.stream_index+1}
      | _, '>' -> { buf with tail_index = buf.stream_index+1; last_rule=Token.index_of_token (Token.REGEX "") }
      | _, c when c = eof -> failwith "Unmatched '<'"
      | _ -> regex_state curr {buf with stream_index= buf.stream_index+1}

    and string_state (prev : char) ( buf : lexbuf ) : lexbuf =
      let curr = (peek buf) in
      match prev, curr with
      | '\\', '\"' -> string_state curr {buf with stream_index= buf.stream_index+1}
      | _, '\"' -> { buf with tail_index = buf.stream_index+1; last_rule=Token.index_of_token (Token.STRING "") }
      | _, c when c = eof -> failwith "Unmatched '\"'"
      | _ -> string_state curr {buf with stream_index= buf.stream_index+1}

    in

    (* Change to an appropriate token if it is an Identifier token, and the "id" matches another token*)
    let cleanup (tok : Token.token) : Token.token = 
      match tok with
      | ID id -> 
      (
        match id with
        | "rule" -> Token.RULE
        | "parse" -> Token.PARSE
        | "as" -> Token.AS
        | "eof" -> Token.EOF_PATT
        | _ -> tok
      )
      | _ -> tok
    in

    (* Update the buffer with a new head index, and clear the last rule value *)
    let buf = { buf with head_index = buf.tail_index; last_rule=no_rule } in
    let buf = scan_state buf in
    let token = Token.make_token buf.last_rule (matched_of_lexbuf buf) in
    let token = cleanup token in
    let buf = { buf with stream_index = buf.tail_index; } in
    token, buf


    let lexAll ( fileContents : string ) : Token.token list = 
      (* conver the string to a buffer *)
      let buf = lexbuf_of_string fileContents in 
    
      (* define a helper function to parse until we see an eof token, constructing 
        a reversed list and returing this list *)
      let rec helper (buf : lexbuf) (list : Token.token list) : Token.token list =
      let tok, buf = tokenize buf in
      match tok with 
      | Token.EOF -> (tok :: list)
      | _ -> helper buf ( tok :: list )
  in

  (* call the helper and reverse its output *)
  List.rev ( helper buf [] )


  let tokenize2 (str : string) (start : int)= 
    let len = String.length str in
    let pos = ref start in 
  
    let next() = if !pos < len then Some str.[!pos] else None in 
    let peek() = if (!pos + 1) < len then Some str.[!pos+1] else None in
    
    let adv() = pos := !pos + 1 in 

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
      | Some (' ' | '\n' | '\t') -> let _ = adv () in lex ()
      | Some ':' -> Token.COLON
      | Some '|' -> Token.BAR
      | Some '*' -> Token.STAR
      | Some '(' -> Token.LPAREN
      | Some ')' -> Token.RPAREN
      | Some '[' -> Token.LBRACKET
      | Some '-' -> Token.DASH
      | Some ']' -> Token.RBRACKET
      | Some '=' -> Token.EQUALS
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
        | false,_ -> failwith (Printf.sprintf "Unmatched ''' in '%s" matched)
        | _, 4 when matched.[1] = '\\' -> Token.CHAR (MyUtil.escape matched.[2])
        | _, 3 -> Token.CHAR (matched.[1])
        | _ -> failwith (Printf.sprintf "'%s' is not a character." matched)
      )
      | Some '{' -> 
      (
        let ident = ref 0 in 
        let continue = fun c -> 
          match c with 
          | Some '}' when !ident = 0 -> false
          | Some '}' when !ident > 0 -> let _ = ident := !ident-1 in true
          | Some '{' -> let _ = ident := !ident+1 in true          
          | None -> false
          | _ -> true
        in let matched,found = match_while continue in 
        if found then Token.CODE(MyUtil.trim matched) 
        else failwith (Printf.sprintf "Unmatched '{' in %s" matched)
      )
     | Some '"' -> 
      ( 
        let continue = fun c -> 
          match c with 
          | Some '"' -> false  
          | None -> false
          | _ -> true
        in let matched,found = match_while continue in 
        if not found then failwith (Printf.sprintf "Unmatched '\"' in %s" matched)  
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
        | _ -> Token.ID(matched)
      )
     | Some c -> failwith (Printf.sprintf "Unexpected character '%c'" c)
    in
    let rec aux toks = 
      let tok = lex () in
      let _ = adv () in
      match tok with 
      | Token.EOF -> List.rev (tok :: toks)
      | _ -> aux (tok :: toks)
    in
    aux []
end