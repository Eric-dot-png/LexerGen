(*
  MyUtil.ml
  --------------
  Small helper module used by other modules in this project.
*)

module MyUtil = struct
  let is_alpha (c : char) : bool = 
    let code = Char.code c in
    (code >= Char.code 'a' && code <= Char.code 'z') ||
    (code >= Char.code 'A' && code <= Char.code 'Z')
  
  let is_digit (c : char) : bool =
    let code = Char.code c in
    (code >= Char.code '0' && code <= Char.code '9')

  let is_alnum (c : char) : bool = (is_digit c) || (is_alpha c)

  
end