type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let hashDir _obj =
  match _obj with
    (*Si c'est juste du text on le renvoi dans une liste*)
    | Text s -> [s]
    (*si c'est un dir on va le parcourir par récursion et en faire une liste*)
    | Directory dir -> let rec loop aux = function
      (*si le dir est vide on renvoi notre liste avec tout les noms bool et hash*)
      | [] -> aux
      (*On force le format main.ml;t;94daecdffe4003a70f02ee8989295b32 et on ajoute tout ça dans une liste par récursion*)
      | (nom, is_dir, digest, _) :: tl -> loop ((nom ^ ";" ^ (if is_dir then "d" else "t") ^ ";" ^ (Digest.to_hex digest)) :: aux) tl
    (*On doit reverse la liste sinon ça marche pas ta vu*)
    in List.rev (loop [] dir)

(*Mtn on va juste digest ce que nous a renvoyé la fonction d'avant*)
let hash _obj = Digest.string (String.concat "\n" (hashDir _obj))

let is_known (_h:Digest.t) = 
  begin
    (*Chemin général a vérif sinon finito*)
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/repo/.ogit/objects";
    (*On va vérifier si le fichier existe, on se base sur le nom hash*)
  if Sys.file_exists (Digest.to_hex _h ) then true else false
  end

(*Normalement elle marche mais c'est pas neuff, les tests dune ne l'aiment pas*)
let read_text_object _h = 
  begin 
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    (*On ouvre le fichier en lecture binaire, je penses que ça peut eviter des soucis avec les accents etc*)
  let ic = open_in_bin (_h) in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (*on fini le taff et return la chaine qui correspond a tout le contenue du fichier*)
  s
  end

(*A refaire ça pue sa mère mais l'idée est la*)
let store_object _obj = match _obj with
  | Text txt -> begin 
    let err = Sys.command ("touch " ^ (Digest.to_hex(hash _obj)) ^ " && echo \"" ^ txt ^ "\" > " ^ (Digest.to_hex (hash _obj))) in
    if err <> 0 then failwith "erreur" else
    Digest.string txt
  end
  | Directory dir -> match dir with
    |[] -> begin
      let tmp = Digest.to_hex (Digest.string "") in
      let err = Sys.command ("touch " ^ tmp) in
      if err <> 0 then failwith "erreur" else
      Digest.string ""
      end
    | _ -> begin
      let err = Sys.command ("touch " ^ Digest.to_hex(hash _obj) ^ " && echo \"" ^ (String.concat "\n" (hashDir _obj)) ^ "\" > " ^ (Digest.to_hex (hash _obj))) in
      if err <> 0 then failwith "erreur" else
      hash _obj
    end;;
  
  (*function
  | Text txt -> begin 
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/repo/.ogit/objects";
    let err = Sys.command ("touch " ^ (Digest.to_hex txt) ^ " && cat " ^ txt ^ " > " ^ (Digest.to_hex txt)) in
    if err <> 0 then failwith "erreur" else
    Digest.to_hex txt
  end
  | Directory _ -> failwith "TODO"
  
  begin
    Unix.chdir "/home/quentin/Ocaml/projet-pf-ogit/.ogit/objects";
    let err = Sys.command ("touch " ^ (Digest.to_hex (hash obj1)) ^ " && echo \"" ^ (String.concat "\n" (hashDir obj1)) ^ "\" > " ^ (Digest.to_hex (hash obj1))) in
    if err <> 0 then failwith "erreur" else
    hash obj1
  end
  
  *)


let store_work_directory () = 

let read_directory_object _h = failwith "TODO" 
  
let clean_work_directory () = failwith "TODO"

let restore_work_directory _obj = failwith "TODO" 

let merge_work_directory_I _obj = failwith "TODO"