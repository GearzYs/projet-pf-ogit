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
    (*On reverse la liste*)
      in List.rev (loop [] dir)

(*Mtn on va juste digest ce que nous a renvoyé la fonction d'avant*)
let hash _obj = Digest.string (String.concat "\n" (hashDir _obj))

let is_known _h =
    (*On va vérifier si le fichier existe, on se base sur le nom hash*)
    Sys.file_exists (".ogit/objects/" ^ (Digest.to_hex _h ))

let write_to_file path msg =
  let oc = open_out path in
  Printf.fprintf oc "%s" msg;
  close_out oc

let store_object _obj = (*On va stocker l'objet dans le dossier .ogit/objects*)
  match _obj with
  | Text txt -> begin
    (*On execute 2 commandes : Touch "hash de l'obj" + Echo du fichier dont la sortie standart est le fichier qu'on a crée*)
    write_to_file (".ogit/objects/"^(Digest.to_hex (hash _obj))) txt;
    Digest.string txt
  end
  | Directory dir -> match dir with
    |[] -> begin (*Si dir vide*)
      let tmp = Digest.to_hex (Digest.string "") in
      write_to_file (".ogit/objects/" ^ tmp) "";
      Digest.string ""
      end
    | _ -> begin (*Si dir non vide*)
      write_to_file (".ogit/objects/" ^ (Digest.to_hex (hash _obj))) (String.concat "\n" (hashDir _obj));
      hash _obj
    end

let read_text_object _h = (*On lit le fichier du hash donné et on le renvoi sous forme de string*)
  begin 
  let ic = open_in (".ogit/objects/" ^ Digest.to_hex _h) in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s
  end

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
  let rec loop aux chem = match aux with
    | [] -> ()
    | (nom, is_dir, _, obj)::tl -> if is_dir then
      begin
        let err = Sys.command ("mkdir " ^ (chem^nom)) in
        if err <> 0 then () else
        loop (match obj with | Directory obj1 -> obj1 | _ -> failwith "Case not possible") (chem^nom^"/");
        loop tl chem
        end
    else
      begin
        let err = Sys.command ("printf \"%s\" \"" ^ (match obj with | Text txt -> txt | _ -> failwith "warn not possible") ^ "\" > " ^ chem ^ nom) in
        if err <> 0 then () else
        loop tl chem
      end
  in loop (match _obj with | Directory dir -> dir | _ -> failwith "not a directory") "./"

let merge_work_directory_I _obj =
  let isConflict = ref true in
  let rec loop path remote = match remote with (*On va parcourir le remote*)
    | Directory d -> (*Si c'est un dossier*)
      if not (Sys.file_exists path) then (*Si le dossier n'existe pas*)
        let err = Sys.command ("mkdir " ^ path) in (*On le crée*)
        if err <> 0 then failwith "erreur" (*Si erreur on fail*)
        else List.iter (fun (name, _, _hash, obj) -> loop (Filename.concat path name) obj) d (*On parcours le dossier*)
    | Text t -> if (Sys.file_exists path) then (*Si le fichier existe*)
      let local = (read_file path) in (*On lit le fichier local*)
          if local <> t then (*Si le fichier local est différent du fichier distant*)
            begin
              write_to_file (path ^ "..cl") local; (*On écrit le fichier local dans un fichier temporaire*)
              write_to_file (path ^ "..cr") t; (*On écrit le fichier distant dans un fichier temporaire*)
              Sys.remove path; (*On supprime le fichier local*)
              Printf.printf "Conflict on %s\n" path; (*On affiche le conflit*)
              isConflict := false;
            end
          else
            write_to_file path t (*On écrit le fichier distant*)
          in begin
            loop "." _obj; (*On parcours le dossier distant*)
            if not !isConflict then print_string "Merge failed\n"; (*Si il y a eu un conflit on affiche un message d'erreur*)
            !isConflict (*On retourne si il y a eu un conflit ou non*)
          end;;

let merge_work_directory_II _obj = 
  let isConflict = ref true in
  let rec loop path remote = match remote with (*on parcourt l'arborescence*)
    | Directory d -> (*si c'est un dossier*)
      if not (Sys.file_exists path) then (*si le dossier n'existe pas*)
        let err = Sys.command ("mkdir " ^ path) in (*on le crée*)
        if err <> 0 then failwith "erreur" (*si erreur on fail*)
      else List.iter (fun (name, _, _hash, obj) -> loop (Filename.concat path name) obj) d (*on parcours les sous-dossiers*)
    | Text t -> if (Sys.file_exists path) then (*si le fichier existe*)
      let local = (read_file path) in (*on lit le fichier local*)
          if local <> t then (*si le fichier local est différent du fichier remote*)
            begin (*on crée un fichier avec les 3 versions*)
              write_to_file (path ^ "..cl") local; (*local*)
              write_to_file (path ^ "..cr") t; (*remote*)
              Sys.remove path; (*on supprime le fichier local*)
              let err = Sys.command ("diff3 -m " ^ path ^ "..cl " ^ path ^ "..cr " ^ path ^ "..cr > " ^ path) in (*http://manpagesfr.free.fr/man/man1/diff3.1.html*)
              (*on utilise diff3 pour créer un fichier avec les 3 versions*)
              if err <> 0 then failwith "erreur" else
              ();
              Sys.remove (path ^ "..cl");
              Sys.remove (path ^ "..cr");
              Printf.printf "Conflict on %s\n" path; (*on affiche le conflit*)
              isConflict := false;
            end
          else
            write_to_file path t (*on écrit le fichier remote*)
          in begin
            loop "." _obj;
            if not !isConflict then print_string "Merge failed\n"; (*si il y a eu un conflit on affiche un message d'erreur*)
            !isConflict (*On retourne si il y a eu un conflit ou non*)
          end;;