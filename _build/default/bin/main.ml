(*
file  : main.ml
brief : app entry point
*)

open MyLexing
open Token


let rec tokenize_and_print (buf : MyLexing.lexbuf ) = 
  let tok, buf = MyLexing.tokenize buf in 
  let _ = (Printf.printf "Token: < %s > StreamIndex: < %d >\n" (Token.string_of_token tok) buf.stream_index) in
  match tok with
    Token.EOF -> ()
  | _ -> tokenize_and_print buf
;;

  
let () = 
  let str = "|={}" in
  let buf = MyLexing.lexbuf_of_string str in
  let _ = tokenize_and_print buf in ()