(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () = 
  try 
        let _ = Sys.is_directory ".ogit" in ()
    with Sys_error _ -> Sys.command ("mkdir .ogit && mkdir .ogit/logs && mkdir .ogit/objects && touch .ogit/HEAD.txt && echo \"tree 00 > .ogit/objects/0") |> ignore

  (*add first empty commit*)
  (*let _ = Sys.command "echo \"tree 00 > .ogit/objects/0" in
  ()*)
  (*Logs.init_commit ();;*)

let is_present file_name = 
  begin
    try 
      let path = ".ogit/objects/" ^ file_name in
      Sys.file_exists path
    with Sys_error _ -> false
  end

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
