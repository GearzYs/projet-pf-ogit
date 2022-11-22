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
  let tmp = Logs.make_commit _msg (Objects.store_work_directory ()) in
  let head = Logs.store_commit tmp in
  Logs.set_head [Digest.string head]

let ogit_checkout _hash = 
(*Ouvrir logs du hash, lire le fichier, parcourir tout et remplacer*)
  if Sys.file_exists( ".ogit/logs/" ^_hash) then
    let actualCommit = Logs.read_commit _hash in
    let _obj = Objects.read_directory_object actualCommit.content in
    let _ = Objects.restore_work_directory _obj
    in Logs.set_head [_hash]

  else raise (Failure "Hash inconnu")

let ogit_log () = 
  let _ = Sys.command "cat .ogit/logs/HEAD" in
  () (*Logs.read_commit _h*)

let ogit_merge _hash = failwith "TODO"*)
