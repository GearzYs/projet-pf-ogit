(** fichier ogit.ml : le coeur de l'exécutable: parse la ligne de commande et appelle
    les commandes correspondantes (cf https://v2.ocaml.org/api/Arg.html) **)


(*Demander prof comment faire ça *)
open Ogitlib
let ogit_root = "../ogit"
let verbose = ref false
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
    | _ -> failwith "Command not found"

let main =
    begin
        let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                        ("-n", Arg.String (set_argument_donne), "Init | commit | checkout | log | merge ( a ajouter)");
                        ("-d", Arg.String (set_message), "Messade de commit");]
                        
    in let usage_msg = "Ogit "
    in Arg.parse speclist set_argument_donne usage_msg;
    check !argument_donne
    end
    

let () = main
