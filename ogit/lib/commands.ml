(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

(*voir comment compiler pour utiliser les modules*)
let ogit_init () = 
  Ogit.*

let ogit_commit _msg = 
  begin
    let dir = Sys.readdir ".";
    let tmp = ref list;
    for i = 0 to Array.length dir - 1 do
      if not (is_present dir.(i)) then
        tmp := dir.(i) :: !tmp
    done;
    
  (*Logs.make_commit _s _h*)

let ogit_checkout _hash = failwith "TODO"

let ogit_log () = 
  let _ = Sys.command "cat .ogit/logs/HEAD" in
  () (*Logs.read_commit _h*)

let ogit_merge _hash = failwith "TODO"
