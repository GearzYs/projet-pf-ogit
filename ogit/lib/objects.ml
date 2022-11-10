type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let hash _obj : t = 
  let rec hash_aux obj = 
    match obj with
    | Text s -> Text (Digest.to_hex (Digest.string s))
    | Directory l ->
      let l = List.map (fun (name, is_dir, digest, obj) ->
        (name, is_dir, digest, hash_aux obj)) l in
      Directory l
  in
  hash_aux _obj;;

let is_known (_h:Digest.t) = 
  begin
    (*Chemin général a vérif sinon finito*)
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/repo/.ogit/objects";
  if Sys.file_exists (Digest.to_hex _h ) then true else false
  end
let read_text_object _h = 
  begin 
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/repo/.ogit/objects";
  let ic = open_in_bin (_h) in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s
  end
  
let store_object _obj = match _obj with
  | Text txt -> begin 
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/.ogit/objects";
    let err = Sys.command ("touch " ^ (Digest.to_hex(Digest.string txt)) ^ " && cat " ^ txt ^ " > " ^ (Digest.to_hex (Digest.string txt))) in
    if err <> 0 then failwith "erreur" else
    Digest.string txt
  end
  | Directory dir -> failwith "todo"
  
  (*function
  | Text txt -> begin 
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/repo/.ogit/objects";
    let err = Sys.command ("touch " ^ (Digest.to_hex txt) ^ " && cat " ^ txt ^ " > " ^ (Digest.to_hex txt)) in
    if err <> 0 then failwith "erreur" else
    Digest.to_hex txt
  end
  | Directory _ -> failwith "TODO"*)


let store_work_directory () = failwith "TODO"

let read_directory_object _h = failwith "TODO" 
  
let clean_work_directory () = failwith "TODO"

let restore_work_directory _obj = failwith "TODO" 

let merge_work_directory_I _obj = failwith "TODO"