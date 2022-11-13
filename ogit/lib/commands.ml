(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () = 
  let _ = Sys.command "mkdir .ogit" in
  let _ = Sys.command "mkdir .ogit/objects" in
  let _ = Sys.command "mkdir .ogit/logs" in
  (*add first empty commit*)
  let _ = Sys.command "echo \"tree 00 > .ogit/objects/0" in
  ()
  (*Logs.init_commit ();;*)

let ogit_commit _msg = failwith "TODO"
  (*Logs.make_commit _s _h*)

let ogit_checkout _hash = failwith "TODO"

let ogit_log () = 
  let _ = Sys.command "cat .ogit/logs/HEAD" in
  () (*Logs.read_commit _h*)

let ogit_merge _hash = failwith "TODO"