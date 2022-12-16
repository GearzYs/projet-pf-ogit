(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let ogit_init () = 
  begin
  if Sys.file_exists ".ogit" then 
    raise (Failure "ogit: already an ogit repository")
  else
    let err = Sys.command ("mkdir .ogit && mkdir .ogit/logs && mkdir .ogit/objects && touch .ogit/HEAD") in 
    if err <> 0 then raise (Failure "ogit: error while creating the repository");
  let _ = Logs.set_head([Logs.store_commit(Logs.init_commit ())]) in ()
  end

let ogit_commit _msg = 
  let tmp = Logs.make_commit _msg (Digest.to_hex(Objects.store_work_directory ())) in
  if (List.length (Logs.get_head()))>1 then
    let getFiles =
      let rec files res aux chem= match aux with
        | [] -> res
        | hd::tl -> if (Sys.is_directory (Filename.concat chem hd)) then
          res@(files [] (Array.to_list (Sys.readdir (chem ^ hd ^ "/"))) (chem ^ hd ^ "/")) 
        else files (hd::res) tl chem
      in files [] (Array.to_list (Sys.readdir "./")) "./" in
    let rec parcoursFiles = function
      | [] -> Logs.set_head [(Logs.store_commit tmp)]
      | hd::tl -> if ((Filename.check_suffix hd "..cl")||(Filename.check_suffix hd "..cr")) then
        failwith "Conflict files detected, fix them before commiting"
      else parcoursFiles tl
    in parcoursFiles getFiles;
  else Logs.set_head [(Logs.store_commit tmp)]

let better_hash _hash =
  let len = String.length _hash in
  if len < 4 then failwith "Hash must be > 4 characters"
  else
    let hashlist = Sys.readdir ".ogit/logs" in
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
    let actualCommit = Logs.read_commit (Digest.from_hex hashtemp) in
    let _obj = Objects.read_directory_object actualCommit.content in
    let _ = Objects.restore_work_directory _obj
    in Logs.set_head [Digest.from_hex hashtemp]
  else failwith "Hash inconnu"

let ogit_log () =
  let result = ref [] in
  let rec head = function
    | [] -> List.iter (Printf.printf "%s\n") !result
    | h::t -> let rec pparents = function
                | [] -> ()
                | h::t ->   
                try let actualCommit = Logs.read_commit h in 
              result := [Digest.to_hex h ^ " " ^ actualCommit.message] @ (!result);
              pparents actualCommit.parents;
                with _ ->
                result := ["d41d8cd98f00b204e9800998ecf8427e Initial commit"] @ (!result);
                pparents t
              in pparents [h];
      head t
  in head (Logs.get_head ())

let ogit_merge _hash = 
    let hashtemp = Digest.from_hex (better_hash _hash) in
    let actualCommit = Logs.read_commit hashtemp in
        Logs.set_head ((Logs.get_head()) @ actualCommit.parents);
        if Objects.merge_work_directory_I (Objects.read_directory_object actualCommit.content) then
          ogit_commit ("Merge de " ^ (Digest.to_hex hashtemp)) (*Message du commit = Merge de <son hash>*)
        else failwith "Merge impossible"

let ogit_merge_II _hash = 
  let hashtemp = Digest.from_hex (better_hash _hash) in
  let actualCommit = Logs.read_commit hashtemp in
      Logs.set_head ((Logs.get_head() ) @ actualCommit.parents);
      if Objects.merge_work_directory_II (Objects.read_directory_object actualCommit.content) then
        ogit_commit ("Merge de " ^ (Digest.to_hex hashtemp)) (*Message du commit = Merge de <son hash>*)
      else failwith "Merge impossible"
      

let ogit_log_graph () =
    let rec repeatString s n =
      if n = 0 then "" else s ^ repeatString s (n - 1) in
    let result = ref [] in
    let temp = ref 0 in
    let rec head = function
      | [] -> List.iter (Printf.printf "%s\n") !result
      | h::t -> let rec pparents = function
                  | [] -> ()
                  | h::t ->   
                  try let actualCommit = Logs.read_commit h in 
                    if (List.length t)+1 < !temp then
                      begin
                    result:= [(repeatString "| " (!temp-1))^"* "^(String.sub (Digest.to_hex h) 0 7)^ " : " ^actualCommit.message]@ !result;
                    result:= [(repeatString "| " (!temp-2))^"|\\"] @ !result;
                    temp:= (List.length t)+1;
                    pparents actualCommit.parents;
                      end
                    else if (List.length t)+1 > !temp then
                      begin
                    result:= [(repeatString "| " (!temp))^"* "^(String.sub (Digest.to_hex h) 0 7)^ " : " ^actualCommit.message]@ !result;
                    result:= [(repeatString "| " (!temp))^"|/"] @ !result;
                    temp:= (List.length t)+1;
                    pparents actualCommit.parents;
                      end
                  else begin 
                    result:= [(repeatString "| " (!temp-1))^"* "^(String.sub (Digest.to_hex h) 0 7)^ " : " ^actualCommit.message]@ !result;
                    pparents actualCommit.parents;
                  end
                  with _ ->
                    result:= [("* d41d8cd : Initial commit")]@ !result;
                    pparents t
                in pparents [h];
        head t
    in head (Logs.get_head ())