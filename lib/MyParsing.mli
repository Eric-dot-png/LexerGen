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
      | String of string
      | Union of regex * regex
      | Cat of regex * regex
      | Star of regex
      | CharRange of char * char
    type flat_regex =
        FChar of char
      | FString of string
      | FCharRange of char * char
      | FUnion
      | FCat
      | FStar
    val string_of_regex : regex -> string
    val string_of_flat_regex : flat_regex -> string
    val postorder : regex -> flat_regex list * int 
    val parse : Token.Token.token list -> lex_file
  end
