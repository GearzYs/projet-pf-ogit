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
    (*On doit reverse la liste sinon ça marche pas tas vu*)
      in List.rev (loop [] dir)

(*Mtn on va juste digest ce que nous a renvoyé la fonction d'avant*)
let hash _obj = Digest.string (String.concat "\n" (hashDir _obj))

let is_known _h =
    (*Chemin général a vérif sinon finito*)
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    (*decommenter ligne endessous *)
    (*On va vérifier si le fichier existe, on se base sur le nom hash*)
    Sys.file_exists (".ogit/objects/" ^ (Digest.to_hex _h ))

(*a partir dici on est pas sur que ca marche bien*)
(*Semble marcher mais a des soucis avec dune a cause du répertoire courant*)
let store_object _obj = match _obj with
  | Text txt -> begin 
    (*On execute 2 commandes : Touch "hash de l'obj" + Echo du fichier dont la sortie standart est le fichier qu'on a crée*)
    let err = Sys.command ("cd .ogit/objects && printf \"%s\" \"" ^ txt ^ "\"> " ^(Digest.to_hex (hash _obj))) in
    if err <> 0 then failwith "erreur" else (*Si il y a une erreur, le retour de la commande sera différent de 0*)
    Digest.string txt
  end
  | Directory dir -> match dir with
    |[] -> begin (*Si dir vide*)
      let tmp = Digest.to_hex (Digest.string "") in
      let err = Sys.command ("cd .ogit/objects && touch " ^ tmp) in
      if err <> 0 then failwith "erreur" else
      Digest.string ""
      end
    | _ -> begin
      let err = Sys.command ("cd .ogit/objects && printf \"%s\" \"" ^ (String.trim(String.concat "\n" (hashDir _obj))) ^ "\" > " ^ (Digest.to_hex (hash _obj))) in
      if err <> 0 then failwith "erreur" else
      hash _obj
    end;;

(*Normalement elle marche mais c'est pas neuff, les tests dune ne l'aiment pas*)
let read_text_object _h = 
  begin 
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    (*On ouvre le fichier en lecture binaire hex, je penses que ça peut eviter des soucis avec les accents etc*)
  let ic = open_in (_h) in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (*on fini le taff et return la chaine qui correspond a tout le contenue du fichier*)
  s
  end

(*on l'uitilise  pour convertir "repo/" sous forme de liste afin d'itérer desssus*)
let dir_to_list dir =
  let rec loop acc = function
    | [] -> acc
    | hd :: tl ->
      let acc = if Sys.is_directory hd then loop acc (Sys.readdir hd |> Array.to_list |> List.map (Filename.concat hd)) else hd :: acc in
      loop acc tl
  in
  loop [] [dir]

let store_work_directory () = 
  let contenu = dir_to_list "repo/" in
  let rec loop res aux= match aux with
    | [] -> String.concat "\n" (List.rev res)
    | hd::tl -> if not(Sys.is_directory hd) then
      let hash1 = hash (Text(read_text_object hd)) in
      if not(is_known hash1) then
        loop ((store_object (Text (read_text_object hd)))::res) tl
      else loop res tl
    else loop res tl
  in loop [] contenu

let dir_file_in_list _h = 
  if is_known (Digest.from_hex _h) then
    let file = read_text_object _h in
    let rec loop res aux = match aux with
      | [] -> res
      | hd::tl -> let tmp = String.split_on_char ';' hd in
        loop ((List.nth tmp 0, List.nth tmp 1, List.nth tmp 2)::res) tl
    in loop [] (String.split_on_char '\n' file)
  else failwith "not a known object"

let rec read_directory_object _h =
  
let clean_work_directory () = Sys.command("find repo/ -type f -name \"[^.]*\" -delete")

let restore_work_directory _obj = failwith "TODO" 

let merge_work_directory_I _obj = failwith "TODO"