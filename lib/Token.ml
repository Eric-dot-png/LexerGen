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

    (** [hash tok] is the hash function to hash tokens.
        @param tok token that must be an identifier token
        @return the hash value for the identifier string contained by tok *)
    let hash (tok : token) = 
      match tok with
      | ID(id) -> Hashtbl.hash id 
      | _ -> MyUtil.fmt_failwith "hash only hashes ids, not \"%s\"" 
          (string_of_token tok)
end

(*
module IdKey = struct
  open MyUtil
  open Token

  type t = Token.token
  let equal (tok1 : t) (tok2 : t) = 
    match tok1, tok2 with
    | ID(id1), ID(id2) -> id1 = id2
    | _ -> MyUtil.fmt_failwith "TokenKey.equal : Unexpected tokens : %s" 
      ((Token.string_of_token tok1)^", "^(Token.string_of_token tok2))

  let hash tok = 
    match tok with 
    | ID(id) -> Hashtbl.hash(id)
    | _ -> MyUtil.fmt_failwith "TokenKey.hash : Unexpected token : %s" 
      (Token.string_of_token tok)
end

module SymTable = Hashtbl.Make(IdKey)
*)