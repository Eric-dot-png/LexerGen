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

  let head (list : 'a list) : 'a * 'a list = 
    match list with 
      head::tail -> head,tail
    | [] -> failwith "MyUtil.head : list is empty"
 
  let read_file ( file_path : string ) : string = 
    In_channel.with_open_text file_path In_channel.input_all

  let write_file ( file_path : string ) ( content : string ) : unit =
    Out_channel.with_open_text file_path (fun oc -> Out_channel.output_string oc content)

end