(** fichier ogit.ml : le coeur de l'exécutable: parse la ligne de commande et appelle
    les commandes correspondantes (cf https://v2.ocaml.org/api/Arg.html) **)

open Ogitlib
let root = "../repo"
let argument_donne = ref ""
let commit_message = ref ""
let set_argument_donne s = argument_donne := s
let set_message s = commit_message := s
let check s = match s with 
    (*Ne trouve pas le module command*)
    | "init" -> Commands.ogit_init ()
    | "commit" -> Commands.ogit_commit !commit_message
    | "checkout" -> Commands.ogit_checkout !argument_donne
    | "log" -> Commands.ogit_log ()
    | "merge" -> Commands.ogit_merge !argument_donne
    | "bettermerge" -> Commands.ogit_merge_II !argument_donne
    | _ -> failwith "Command not found"

let main =
    begin
        Sys.chdir root;
        let speclist = [("-c | -command", Arg.String (set_argument_donne), "init | commit | checkout | log | merge | bettermerge");
                        ("-m | -message", Arg.String (set_message), "Message du commit");] in                        
    in Arg.parse speclist set_argument_donne "Ogit ";
    check !argument_donne
    end
    
let () = main
