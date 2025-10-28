module MyParsing :
  sig
    type lex_file = { header : string; trailer : string; rule : rule; }
    and rule = {
      name : string;
      return_type : string;
      none_code : string;
      eof_code : string;
      cases : case list;
    }
    and case = { alias : string; code : string; regex : regex; }
    and regex =
        Emptyset
      | Eof
      | Char of char
      | Literal of string
      | Union of regex * regex
      | Concat of regex * regex
      | Star of regex
      | Charset of char * char
    type flat_regex =
        FChar of char
      | FLiteral of string
      | FCharset of char * char
      | FUnion
      | FConcat
      | FStar
    val string_of_regex : regex -> string
    val string_of_flat_regex : flat_regex -> string
    val postorder : regex -> flat_regex list * int * int
    val parse : Token.Token.token list -> lex_file
  end
