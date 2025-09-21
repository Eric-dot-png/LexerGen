(* 
file  : MyLexing.ml 
brief : Provide meta-lexer speciifcation utilities
*)

module MyLexing = struct
  open Token

  type lexbuf = {
    cstream : string; (* stream of characters *)
    stream_index : int; (* index in the stream *)
    head_index : int; (* head/read index *)
    tail_index : int; (* tail/consume index *) 
    last_rule : int; (* last matched rule *)
 }

  let matched_of_lexbuf ( buf : lexbuf ) : string = 
    ( String.sub buf.cstream buf.head_index (buf.tail_index-buf.tail_index) )

    
  let eof = (char_of_int 0) (* eof '\0' constant char *)
  let no_rule = -1 (* no rule constant char *)
  let dead_state = -1 (* dead state constant char *)

  let lexbuf_of_string ( cstream : string ) : lexbuf = 
    { cstream=cstream; stream_index = 0; head_index=0; tail_index=0; last_rule= (-1); }

  let peek ( buf : lexbuf ) : char =
    if buf.stream_index < (String.length buf.cstream) then
      buf.cstream.[buf.stream_index] 
     else eof 
     
   let tokenize ( buf : lexbuf ) : Token.token * lexbuf =  
    let rec scan_state (buf : lexbuf ) : lexbuf = 
      (* (starting) state that scans for rules *)
      match (peek buf) with
        '|' -> 
          { buf with 
            tail_index = buf.stream_index; 
            last_rule = Token.index_of_token( Token.RULE );
            }
      | '=' -> 
        { buf with
          tail_index = buf.stream_index;
          last_rule = Token.index_of_token( Token.EQUALS );
        }
      | '{' -> 
        let buf = { buf with stream_index = buf.stream_index+1 } in 
        code_state 0 buf 
      | c when c = eof -> { buf with  last_rule =  Token.index_of_token( Token.EOF ); }
      | _ -> failwith "Not implemented"
    and code_state (indent : int) (buf : lexbuf) =
      (* state that handles code block logic *) 
      match (peek buf) with
        '{' ->
        code_state (indent+1) {buf with stream_index=buf.stream_index+1;}
      | '}' ->
        if indent = 0 then { buf with 
          tail_index = buf.stream_index;
          last_rule  = Token.index_of_token( Token.CODE(""));
        }
        else code_state (indent-1) {buf with stream_index=buf.stream_index+1;}
      | c when c = eof -> failwith "Unmatched '{'" 
      | _ -> 
        code_state indent {buf with stream_index=buf.stream_index+1;}  
    in
    let buf = { buf with head_index = buf.tail_index; } in 
    let buf = (scan_state buf) in 
    let _ = Printf.printf "head_index=%d, tail_Index=%d\n" buf.head_index buf.tail_index in
    let token = Token.make_token buf.last_rule ( matched_of_lexbuf buf ) in
    let _ = print_endline "Here..." in
    let buf = { buf with stream_index = buf.tail_index+1; head_index=buf.tail_index+1} in
    token,buf 

  end