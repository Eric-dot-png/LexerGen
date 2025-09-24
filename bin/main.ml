(*
file  : main.ml
brief : app entry point
*)

open MyLexing
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



let () = 
  let str = sample_file in
  let toks = MyLexing.lexAll str in
  let lex_file = MyParsing.parse toks in 
  let _ = MyParsing.print_lex_file lex_file in 
  () 