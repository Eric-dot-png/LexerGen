(*
file  : main.ml
brief : app entry point
*)

open MyLexing
open Token


let rec tokenize_and_print (buf : MyLexing.lexbuf ) = 
  let tok, buf = MyLexing.tokenize buf in 
  let _ = (Printf.printf "Token: %s Lexbuf: %s\n" (Token.string_of_token tok) (MyLexing.string_of_lexbuf buf)) in
  match tok with
    Token.EOF -> ()
  | _ -> tokenize_and_print buf
;;

  
let () = 
  let str = "|={ return {1,2,3}; } abc123ABC" in
  let buf = MyLexing.lexbuf_of_string str in
  let _ = tokenize_and_print buf in ()