(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

(*voir comment compiler pour utiliser les modules*)

let ogit_init () = 
  begin
  if Sys.file_exists ".ogit" then 
    raise (Failure "ogit: already an ogit repository")
  else
    let err = Sys.command ("mkdir .ogit && mkdir .ogit/logs && mkdir .ogit/objects && touch .ogit/HEAD") in 
    if err <> 0 then raise (Failure "ogit: error while creating the repository");
  let _ = Logs.init_commit in ()
  end

let ogit_commit _msg = 
  (*ogit commit “<description>” : parcourt récursivement l’arbre de travail et
    ajoute tous les nouveaux états rencontrés, puis ajoute le commit correspondant 
    avec pour parent le commit HEAD. Les fichiers dont le nom commence 
    par un point (e.g. “.mvn”) sont ignorés*)
  let tmp = Logs.make_commit _msg (Digest.to_hex(Objects.store_work_directory ())) in
  let head = Logs.store_commit tmp in
  Logs.set_head [head]
  
let better_hash _hash =
  let len = String.length _hash in
  if len < 4 then failwith "Hash must be > 4 characters"
  else
    let hashlist = Sys.readdir ".ogit/objects" in
    let rec aux i res=
      if i = Array.length hashlist then match (List.length res) with
        | 0 -> failwith "No match found"
        | 1 -> List.hd res
        | _ -> failwith "Multiple matches found"
      else
        let hash = hashlist.(i) in
        if String.sub hash 0 len = _hash then aux (i+1) (hash::res)
        else aux (i+1) res
    in aux 0 []
      
let ogit_checkout _hash = 
(*Ouvrir logs du hash, lire le fichier, parcourir tout et remplacer*)
  let hashtemp=better_hash _hash in
  if Sys.file_exists( ".ogit/logs/" ^hashtemp) then
    let actualCommit = Logs.read_commit hashtemp in
    let _obj = Objects.read_directory_object actualCommit.content in
    let _ = Objects.restore_work_directory _obj
    in Logs.set_head [hashtemp]
  else failwith "Hash inconnu"

let ogit_log () = 
  let rec aux hd = 
    if hd = [] then ()
    else
      let commit = Logs.read_commit (List.hd hd) in
      let _ = print_endline ("commit " ^ Digest.to_hex(commit.content) ^ " " ^ commit.message) in
      aux (commit.parents)
  in aux (Logs.get_head ())
  

(*
voir marie ou lozes pour savoir ou placer better hash, merge2, log graph dans les fichiers
voir aussi bug du nth et du failure dans read_directory_object


let ogit_merge _hash = 
    let hashtemp = better_hash _hash in
*)
