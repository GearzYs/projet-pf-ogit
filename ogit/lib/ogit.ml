(** fichier ogit.ml : le coeur de l'exécutable: parse la ligne de commande et appelle
    les commandes correspondantes (cf https://v2.ocaml.org/api/Arg.html) **)
(*"mkdir .ogit && mkdir .ogit/logs && mkdir .ogit/objects && touch .ogit/HEAD.txt"*)

let ogit_root = "../ogit"

let () = 
    try 
        let _ = Sys.is_directory ".ogit" in ()
    with Sys_error _ -> Sys.command ("mkdir .ogit && mkdir .ogit/logs && mkdir .ogit/objects && touch .ogit/HEAD.txt") |> ignore