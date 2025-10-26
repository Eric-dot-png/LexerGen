module MyUtil :
  sig
    val escape : char -> char
    val descape : char -> string
    val is_alpha : char -> bool
    val is_digit : char -> bool
    val is_alnum : char -> bool
    val is_alnum_ex : char option -> bool
    val head : 'a list -> 'a * 'a list
    val read_file : string -> string
    val write_file : string -> string -> unit
    val trim : string -> string
    val fmt_failwith : ('a -> string, unit, string) format -> 'a -> 'b
  end
