(*
  Token.ml
  --------
  Lightweight token definitions used by the small meta-lexer.

  The module exposes:
  - the [token] variant describing all token kinds produced by the
    meta-lexer, and
  - small helpers to map between token constructors and a stable numeric
    index (used by other parts of the project), plus a human-friendly
    [string_of_token].
*)

module Token = struct
  open MyUtil

  (* ------------------------------------------------------------------ *)
  (* Token type                                                            *)
  (* ------------------------------------------------------------------ *)

  (** Tokens produced by the meta-lexer. Some carry a string payload
      (e.g. identifiers or code blocks). *)
  type token =
    | RULE
    | ID of string
    | EQUALS
    | BAR
    | CODE of string
    | STRING of string
    | AS
    | EOF_PATT
    | PARSE
    | NONE_PATT
    | EOF
    | COLON
    | CHAR of char 
    | STAR
    | LPAREN
    | RPAREN
    | LBRACKET
    | DASH
    | RBRACKET
    | LET
    | PLUS
    | CAROT

  type t = {
    kind      : token;
    abs_start : int;
    len       : int;
    line      : int; 
    column    : int;
  }

  (* ------------------------------------------------------------------ *)
  (* Pretty printing                                                       *)
  (* ------------------------------------------------------------------ *)

  (** Return a human-readable representation of a token. This is useful
      for debugging and tests. Payloads are included verbatim. *)
  let string_of_token (tok : token) : string =
    match tok with
    | RULE -> "RULE"
    | ID id -> "ID(" ^ id ^ ")"
    | EQUALS -> "EQUALS"
    | BAR -> "BAR"
    | CODE code -> "CODE(" ^ code ^ ")"
    | STRING s -> "STRING(" ^ s ^ ")"
    | AS -> "ALIAS"
    | EOF_PATT -> "EOF_PATT"
    | PARSE -> "PARSE"
    | NONE_PATT -> "NONE_PATT"
    | EOF -> "EOF"
    | COLON -> "COLON"
    | CHAR c -> (Printf.sprintf "CHAR('%s')" (MyUtil.descape c))
    | STAR -> "STAR"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACKET -> "LBRACKET"
    | DASH -> "DASH"
    | RBRACKET -> "RBRACKET"
    | LET -> "LET"
    | PLUS -> "PLUS"
    | CAROT -> "CAROT"
    
end
