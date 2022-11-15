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
    Unix.chdir "main/.ogit/objects";
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

  
 (*a partir dici on est pas sur que ca marche bien*)
(*Semble marcher mais a des soucis avec dune a cause du répertoire courant*)
let store_object _obj = match _obj with
  | Text txt -> begin 
    (*On execute 2 commandes : Touch "hash de l'obj" + Echo du fichier dont la sortie standart est le fichier qu'on a crée*)
    let err = Sys.command ("cd main/.ogit/objects && echo -n \"" ^ txt ^ "\"> " ^(Digest.to_hex (hash _obj))) in
    if err <> 0 then failwith "erreur" else (*Si il y a une erreur, le retour de la commande sera différent de 0*)
    Digest.string txt
  end
  | Directory dir -> match dir with
    |[] -> begin (*Si dir vide*)
      let tmp = Digest.to_hex (Digest.string "") in
      let err = Sys.command ("cd main/.ogit/objects && touch " ^ tmp) in
      if err <> 0 then failwith "erreur" else
      Digest.string ""
      end
    | _ -> begin
      let err = Sys.command ("cd main/.ogit/objects && echo -n \"" ^ (String.trim(String.concat "\n" (hashDir _obj))) ^ "\" > " ^ (Digest.to_hex (hash _obj))) in
      if err <> 0 then failwith "erreur" else
      hash _obj
    end;;

let rec convert_dir_to_obj _dir = match _dir with
  | [] -> []
  | (nom, is_dir, digest, _) :: tl -> (nom, is_dir, digest, (if is_dir then convert_dir_to_obj _dir else Text (read_text_object (Digest.to_hex digest)))) :: (convert_dir_to_obj tl);;

let store_work_directory () = (*unit->string*)
  let dir = Sys.readdir "." in
  let rec loop aux = function
    | [] -> aux
    | hd :: tl -> loop ((hd, Sys.is_directory hd, Digest.file hd, (if Sys.is_directory hd then store_work_directory () else Text (read_text_object (Digest.to_hex (Digest.file hd)))) ) :: aux) tl
  in Directory (loop [] (Array.to_list dir));;

(*On va chercher le répertoire courant*)

(*On va juste lire le fichier et on va le renvoyer dans un objet de type t*)

(*On va juste lire le fichier et le renvoyer*)

let read_directory_object _h = failwith "TODO"

(*On va chercher le répertoire courant*)

(*On va juste lire le fichier et on va le renvoyer dans un objet de type t*)
  
let clean_work_directory () = failwith "TODO"

let restore_work_directory _obj = failwith "TODO" 

let merge_work_directory_I _obj = failwith "TODO"