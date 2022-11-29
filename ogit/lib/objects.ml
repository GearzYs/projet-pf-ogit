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
    end

(*Normalement elle marche mais c'est pas neuff, les tests dune ne l'aiment pas*)
let read_text_object _h = 
  begin 
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    (*On ouvre le fichier en lecture binaire hex, je penses que ça peut eviter des soucis avec les accents etc*)
  let ic = open_in (".ogit/objects/" ^ Digest.to_hex _h) in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (*on fini le taff et return la chaine qui correspond a tout le contenue du fichier*)
  s
  end

(*on l'uitilise  pour convertir "repo/" sous forme de liste afin d'itérer desssus*)
(*let dir_to_list dir =
  let rec loop acc = function
    | [] -> acc
    | hd :: tl ->
      let acc = if (Sys.is_directory hd) then loop acc (Sys.readdir hd |> Array.to_list |> List.map (Filename.concat hd)) else hd :: acc in
      loop acc tl
  in
  loop [] [dir]*)

let read_file _path = (*On lit le fichier et on le renvoi sous forme de string*)
  let ic = open_in _path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let read_dir _path = (*On lit le dir et on le renvoi sous forme de liste de string*)
  let contenu = List.rev (Array.to_list (Sys.readdir _path)) in
    let rec loop res acc = match acc with
    | [] -> res
    | hd::tl -> if (String.get hd 0 <> '.') then loop (hd::res) tl else loop res tl
  in loop [] contenu

let rec dir_to_t_object _path = (*On converti un dir en t_object*)
  let rec loop res acc = match acc with
  | [] -> List.rev res
  | hd::tl -> if (Sys.is_directory (Filename.concat _path hd)) then let dirtemp = Directory (dir_to_t_object (Filename.concat _path hd)) in
  loop ((hd, true, hash dirtemp, dirtemp)::res) tl 
  else loop ((hd, false, Digest.file (Filename.concat _path hd), Text (read_file (Filename.concat _path hd)))::res) tl
in loop [] (read_dir _path)

let store_work_directory () = (*On va lire le répertoire courant et on va le stocker*)
  let contenu = read_dir "./" in
  let rec loop res aux chem= match aux with
    | [] -> store_object (Directory (dir_to_t_object "./"))
    | hd::tl -> if not(Sys.is_directory (Filename.concat chem hd)) then
      let hash1 = hash (Text(read_file (Filename.concat chem hd))) in
      if not(is_known hash1) then
        loop ((store_object (Text (read_file (Filename.concat chem hd))))::res) tl chem
      else loop (hd::res) tl chem
    else loop ((store_object (Directory (dir_to_t_object (Filename.concat chem hd))))::res) (read_dir (Filename.concat chem hd)) (chem ^ hd ^ "/")
  in loop [] contenu "./"

let dir_file_in_list _h = (*On va convertir le contenu du fichier en liste*)
  if is_known _h then
    let file = read_text_object _h in
    let rec loop res aux = match aux with
      | [] -> res
      | hd::tl -> let tmp = String.split_on_char ';' hd in
        loop ((List.nth tmp 0, List.nth tmp 1, List.nth tmp 2)::res) tl
    in loop [] (String.split_on_char '\n' file)
  else failwith "Le fichier n'existe pas"

let rec read_directory_object _h = (*On va lire le fichier et on va le stocker*)
  let rec loop res aux chem = 
      match aux with
      | [] -> Directory(List.rev res)
      | (nom, is_dir, hash)::tl -> if is_dir = "d" then 
        loop ((nom, true, Digest.from_hex hash, read_directory_object (Digest.from_hex hash))::res) tl (chem^nom^"/") 
      else
        loop ((nom, false, Digest.from_hex hash, Text(read_text_object (Digest.from_hex hash)))::res) tl chem
    in loop [] (dir_file_in_list _h) "./" (*obj non reconnu*) 

let clean_work_directory () = 
  let err=Sys.command("find . -not -path \"*/.*\" -delete") in
  if err <> 0 then failwith "erreur" else
  ()

(** écrit dans repo/ 
    tous les fichiers mentionnés dans l'objet passé en paramètre 
    val restore_work_directory : t -> unit
**)

let restore_work_directory _obj = 
  (*voir marie ou lozes car on ne comprend pas comment fix le soucis 
    on n'arrive pas a recrer les sous doss et les fichiers*)  
  let rec loop aux chem = match aux with
    | [] -> ()
    | (nom, is_dir, _, obj)::tl -> if is_dir then
      begin
        let err = Sys.command ("mkdir " ^ (chem^nom)) in
        if err <> 0 then failwith "erreur" else
        loop (match obj with | Directory obj1 -> obj1 | _ -> failwith "Case not possible") (chem^nom^"/");
        loop tl chem
        end
    else
      begin
        let err = Sys.command ("printf \"%s\" \"" ^ (match obj with | Text txt -> txt | _ -> failwith "warn not possible") ^ "\" > " ^ chem ^ nom) in
        if err <> 0 then failwith "erreur" else
        loop tl chem
      end
  in loop (match _obj with | Directory dir -> dir | _ -> failwith "not a directory") "./"
(*
let merge_work_directory_I = failwith "TODO"
*)

let rec repeatString s n =
  if n = 0 then "" else s ^ repeatString s (n - 1)

let log_graph () =
  let logs = Sys.readdir ".ogit/logs" in (*On récupère les logs*)
  let open_all_files = Array.map (fun x -> read_file (".ogit/logs/" ^ x)) logs in (*On ouvre tous les fichiers*)
  let hashs = Array.map (fun x -> List.hd(List.rev(String.split_on_char '\n' x))) open_all_files in (*On récupère les hashs*)
  let finalcomments = Array.map (fun x -> List.nth (List.rev(String.split_on_char '\n' x)) 1) open_all_files in (*On récupère les commentaires*)
  let finalhashs = Array.map (fun x -> String.sub x 0 7) hashs in (*On récupère les 7 premiers caractères des hashs*)
  let branch = Array.map (fun x -> if (List.length (String.split_on_char '\n' x))=4 then 0 else (List.length (String.split_on_char '\n' x))-4 ) open_all_files in (*On récupère les lignes 2 des fichiers*)
  (*let isMerge = liste si merge = true sinon false*)
  let rec result finalcom finalhash merge i res = (*On crée le résultat*)
    if i = Array.length logs then res
    else result finalcom finalhash merge (i+1) (res ^ "*" ^ (repeatString "\t" merge.(i)) ^ finalhash.(i) ^ " : " ^ finalcom.(i) ^ "\n" )
  in result finalhashs finalcomments branch 0 "";;

(*
let read_local_version () =
  let head = read_file ".ogit/HEAD" in
  let logs = read_file (".ogit/logs/"^head) in  
  let hash = String.split_on_char '\n' logs in
  let object1 = read_directory_object (Digest.from_hex (List.nth (List.rev hash) 0)) in
  object1;;

let work = read_directory_object (Digest.from_hex (store_work_directory ()));;

let merge_work_directory_I _obj =
  let local = read_local_version () in
  match _obj with

let merge_work_directory_I _obj = failwith "not implemented"
_obj1 = _obj
_obj2 = read directory (4eme ligne de logs du head)
hash _obj
si le hash est connu, on ne fait rien
match _obj with
| Directory dir -> match dir with
|[] -> ()
|(nom, is_dir, _, obj)::tl -> if is_dir then
voir lozes ou marie comment faire

Lorsque que l’on merge l’état d’un commit X à l’état actuel les modifications concurrentes doivent être fusionnées.
Si un fichier  “fich” est présent dans X mais pas présent dans l’état actuel, on l’ajoute dans l’état actuel
Si un fichier  “fich” est présent dans X et dans l’état actuel, avec le même contenu il ne se passe rien
Si un fichier “fich” est présent dans X et dans l’état actuel, mais avec des contenus différents, on obtient un conflit.
Deux versions du fichier sont créés : “fich..cl” (version “locale”) et “fich..cr” (version “remote” pour X)

Etape 1 => Convertir le hash de la 4eme ligne du logs indiqué dans Head en type T avec read_directory_object 
Etape 2 => Comparer le type t _obj donné en paramètre avec le type T qu'on vient de creer
Etape SI => Si nom de fichier _obj non connu dans l'état actuel, importer le fichier
Sinon Si hash du fichier _obj different état actuel alors erreur conflit creer fichier cl (local) cr (remote) et inserer dans directory 
Sinon rien
*)
