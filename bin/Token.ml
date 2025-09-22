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
    | REGEX of string
    | ALIAS
    | EOF

  (* ------------------------------------------------------------------ *)
  (* Index mapping (stable numeric representation)                       *)
  (* ------------------------------------------------------------------ *)

  (** Return a stable integer id for a token variant. The numeric ids are
      intentionally small and stable so they can be used in compact
      serialized forms or tables elsewhere in the project. *)
  let index_of_token (tok : token) : int =
    match tok with
    | RULE -> 0
    | ID _ -> 1
    | EQUALS -> 2
    | BAR -> 3
    | CODE _ -> 4
    | STRING _ -> 5
    | REGEX _ -> 6
    | ALIAS -> 7
    | EOF -> 8

  (** Construct a token from a numeric index and optional string payload.
      Calling with an unknown index raises [Invalid_argument]. *)
  let make_token (index : int) (str : string) : token =
    match index with
    | 0 -> RULE
    | 1 -> ID str
    | 2 -> EQUALS
    | 3 -> BAR
    | 4 -> CODE str
    | 5 -> STRING str
    | 6 -> REGEX str
    | 7 -> ALIAS
    | 8 -> EOF
    | _ -> invalid_arg "Token.make_token: invalid index"

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
    | REGEX r -> "REGEX(" ^ r ^ ")"
    | ALIAS -> "ALIAS"
    | EOF -> "EOF"

end