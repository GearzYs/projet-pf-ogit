(** fichier ogit.ml : le coeur de l'exécutable: parse la ligne de commande et appelle
    les commandes correspondantes (cf https://v2.ocaml.org/api/Arg.html) **)

let root = "../repo"
let argument_donne = ref []
let commit_message = ref []
let set_argument_donne l = argument_donne := l
let set_message l = commit_message := l
let check l = match l with
    |[]->Printf.printf "No commands\n"
    |s::tl -> match s with
        | "init" -> Commands.ogit_init ()
        | "commit" -> Commands.ogit_commit (List.hd tl)
        | "checkout" -> Commands.ogit_checkout (List.hd tl)
        | "log" -> Commands.ogit_log ()
        | "log_graph" -> Commands.ogit_log_graph ()
        | "merge" -> Commands.ogit_merge (List.hd tl)
        | "bettermerge" -> Commands.ogit_merge_II (List.hd tl)
        | _ -> Printf.printf "Command \"%s\" not found\n" s

let main =
    begin
        Sys.chdir root;
        let speclist = [("-c", Arg.Rest_all (set_argument_donne), "init | commit | checkout | log | merge | bettermerge | log_graph")]
    in Arg.parse speclist test "Ogit ";
    check !argument_donne
    end
    
let () = main
