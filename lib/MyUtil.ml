(*
  MyUtil.ml
  --------------
  Small helper module used by other modules in this project.
*)

module MyUtil = struct
  let escape ( c : char ) : char = 
    match c with 
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | _ -> c

  let descape (c : char) : string = 
    match c with
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\r' -> "\\r"
    | _ -> Printf.sprintf "%c" c

  let is_alpha (c : char) : bool = 
    let code = Char.code c in
    (code >= Char.code 'a' && code <= Char.code 'z') ||
    (code >= Char.code 'A' && code <= Char.code 'Z')
  
  let is_digit (c : char) : bool =
    let code = Char.code c in
    (code >= Char.code '0' && code <= Char.code '9')

  let is_alnum (c : char) : bool = (is_digit c) || (is_alpha c)

  let is_alnum_ex ( c : char option ) : bool = 
    match c with
    | None -> false
    | Some c -> is_alnum c

  let head (list : 'a list) : 'a * 'a list = 
    match list with 
      head::tail -> head,tail
    | [] -> failwith "MyUtil.head : list is empty"
 
  let read_file ( file_path : string ) : string = 
    In_channel.with_open_text file_path In_channel.input_all

  let write_file ( file_path : string ) ( content : string ) : unit =
    Out_channel.with_open_text file_path (fun oc -> Out_channel.output_string oc content)

  let trim (str : string) = String.sub str 1 ( (String.length str) - 2)

end