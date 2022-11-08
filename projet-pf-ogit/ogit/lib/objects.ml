type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let read_file filename =
  let ic = open_in_bin filename in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s;;

let hash _obj = Digest.to_hex (Digest.string (read_file _obj));;

let is_known _h = 
  
let read_text_object _h = failwith "TODO"

let store_object _obj = failwith "TODO"

let store_work_directory () = failwith "TODO"

let read_directory_object _h = failwith "TODO" 
  
let clean_work_directory () = failwith "TODO"

let restore_work_directory _obj = failwith "TODO" 

let merge_work_directory_I _obj = failwith "TODO"