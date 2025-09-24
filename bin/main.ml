(*
file  : main.ml
brief : app entry point
*)

open MyLexing
open Token
open MyParsing


let sample_file = 
"
{
#include <iostream>
}

rule lexer = parse 
| <[a-zA-Z][0-9a-zA-Z_]*> as id { return ID{id}; }
| \"+\" { return PLUS; }

{
int main()
{
  return 0;
}
}

"

let lexAll ( fileContents : string ) : Token.token list = 
  (* conver the string to a buffer *)
  let buf = MyLexing.lexbuf_of_string fileContents in 
  
  (* define a helper function to parse until we see an eof token, constructing 
     a reversed list and returing this list *)
  let rec helper (buf : MyLexing.lexbuf) (list : Token.token list) : Token.token list =
     let tok, buf = MyLexing.tokenize buf in
     match tok with 
     | Token.EOF -> (tok :: list)
     | _ -> helper buf ( tok :: list )
  in

  (* call the helper and reverse its output *)
  List.rev ( helper buf [] )


let print_toks (toks : Token.token list ) : unit =
  let rec helper (toks : Token.token list ) (index : int ) : unit = 
    match toks with 
    | [] -> print_string "\n"
    | tok::toks -> 
      (
        let _ = Printf.printf "%d. %s\n" index (Token.string_of_token tok) in
        helper toks (index+1)
      )
  in
  helper toks 1

let () = 
  let str = sample_file in
  let toks = lexAll str in
  let _ = print_toks toks in 
  let _ = MyParsing.parse toks in 
  () 