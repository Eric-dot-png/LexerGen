(*
file  : main.ml
brief : app entry point
*)

open MyLexing
open Token


let rec tokenize_and_print (buf : MyLexing.lexbuf ) = 
  let tok, buf = MyLexing.tokenize buf in 
  (* let _ = (Printf.printf "Token: %s Lexbuf: %s\n" (Token.string_of_token tok) (MyLexing.string_of_lexbuf buf)) in *)
  let _ = Printf.printf "Token: %s\n" (Token.string_of_token tok) in
  match tok with
  | Token.EOF -> ()
  | _ -> tokenize_and_print buf

let sample_file = 
"
{

}

rule lexer = parse 
| <[a-zA-Z][0-9a-zA-Z_]*> as id { return ID{id}; }
| \"+\" { return PLUS; }

{

}

"


let () = 
  let str = sample_file in
  let buf = MyLexing.lexbuf_of_string str in
  let _ = tokenize_and_print buf in ()