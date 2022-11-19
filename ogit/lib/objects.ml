type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let hashDir _obj =
  match _obj with
    (*Si c'est juste du text on le renvoi dans une liste*)
  | Text s -> [s]
    (*si c'est un dir on va le parcourir par récursion et en faire une liste*)
  | Directory dir -> let rec loop aux = function
      (*si le dir est vide on renvoi notre liste avec tout les noms bool et hash*)
      | [] -> aux
      (*On force le format main.ml;t;94daecdffe4003a70f02ee8989295b32 et on ajoute tout ça dans une liste par récursion*)
      | (nom, is_dir, digest, _) :: tl -> loop ((nom ^ ";" ^ (if is_dir then "d" else "t") ^ ";" ^ (Digest.to_hex digest)) :: aux) tl
    (*On doit reverse la liste sinon ça marche pas tas vu*)
      in List.rev (loop [] dir)

(*Mtn on va juste digest ce que nous a renvoyé la fonction d'avant*)
let hash _obj = Digest.string (String.concat "\n" (hashDir _obj))

let is_known (_h:Digest.t) = 
  
    (*Chemin général a vérif sinon finito*)
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    Unix.chdir ".ogit/objects";
    (*On va vérifier si le fichier existe, on se base sur le nom hash*)
    Sys.file_exists (Digest.to_hex _h )
  
 (*a partir dici on est pas sur que ca marche bien*)
(*Semble marcher mais a des soucis avec dune a cause du répertoire courant*)
let store_object _obj = match _obj with
  | Text txt -> begin 
    (*On execute 2 commandes : Touch "hash de l'obj" + Echo du fichier dont la sortie standart est le fichier qu'on a crée*)
    let err = Sys.command ("cd .ogit/objects && echo -n \"" ^ txt ^ "\"> " ^(Digest.to_hex (hash _obj))) in
    if err <> 0 then failwith "erreur" else (*Si il y a une erreur, le retour de la commande sera différent de 0*)
    Digest.string txt
  end
  | Directory dir -> match dir with
    |[] -> begin (*Si dir vide*)
      let tmp = Digest.to_hex (Digest.string "") in
      let err = Sys.command ("cd .ogit/objects && touch " ^ tmp) in
      if err <> 0 then failwith "erreur" else
      Digest.string ""
      end
    | _ -> begin
      let err = Sys.command ("cd .ogit/objects && echo -n \"" ^ (String.trim(String.concat "\n" (hashDir _obj))) ^ "\" > " ^ (Digest.to_hex (hash _obj))) in
      if err <> 0 then failwith "erreur" else
      hash _obj
    end;;


(*Normalement elle marche mais c'est pas neuff, les tests dune ne l'aiment pas*)
let read_text_object _h = 
  begin 
    (*Pour les test je force ocaml a être dans le dossier de test donné par le prof*)
    (*On ouvre le fichier en lecture binaire hex, je penses que ça peut eviter des soucis avec les accents etc*)
  let ic = open_in (_h) in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (*on fini le taff et return la chaine qui correspond a tout le contenue du fichier*)
  s
  end





(*marche aps

def rec(racine):
    lst=[mes fichiers et dossiers de racine]

    for el in lst:
        if el==file:
            je creer un fichier le titre=mon hash et le contenue est mon contenue

        elif el==dir:
            je creer un fichier le titre=mon hash et pour le contenue la liste de mes fils (fonction que vous avez deja)
            rec(el)

(*marche aps voir khalil pour savoir pk il a fait cette fonction 
let rec convert_dir_to_obj _dir = match _dir with
  | [] -> []
  | (nom, is_dir, digest, _) :: tl -> (nom, is_dir, digest, (if is_dir then convert_dir_to_obj _dir else Text (read_text_object (Digest.to_hex digest)))) :: (convert_dir_to_obj tl);;
*)

match hd with 
        | Text hd -> Sys.command ("cd .ogit/objects && touch " ^ Digest.to_hex (Digest.string (read_text_object hd)) ^ " && echo -n \" " ^ read_text_object hd  ^ "\" > " ^ Digest.to_hex (Digest.string (read_text_object hd)))
        | Directory hd -> Sys.command ("cd .ogit/objects && touch " ^ Digest.string (String.concat "\n" (hashDir hd)))


if Sys.is_directory hd then Sys.command ("cd .ogit/objects && touch " ^ hash hd ) 
      else Sys.command ("cd .ogit/objects && touch " ^ hash hd  ^ " && echo -n \" " ^ read_text_object hd  ^ "\" > " ^ Digest.to_hex (Digest.string (read_text_object hd)))  


let rec store_work_directory () = 
  let contenu = Sys.readdir "repo/" in
  let rec parcours contenu  = match contenu with 
    | [] -> Sys.command ("echo ")
    | hd :: tl -> if Sys.is_directory hd then Sys.command ("cd .ogit/objects && touch " ^ hash hd ) 
      else Sys.command ("cd .ogit/objects && touch " ^ hash hd  ^ " && echo -n \" " ^ read_text_object hd  ^ "\" > " ^ Digest.to_hex (Digest.string (read_text_object hd)))  

*)














(*je capte pas comment parcourir c entrain de me gaver
si c un type text je renvoie le text 
sinon je parcours dir puis si c'est vide je renvoie hash directory vide 
     sinon je regarde si il a une head et taail et je parcours recursivement
     puis je hash dedans quand je peux et je stocke dans une liste   


faut trouver comment convertir tout ce qu'on recup dans variable contenu de store work directory en type t
puis avec ma fonction parcours on pourra parcourir et creer une liste contenu les dossiers et leur contenu hash 
*)


let parcours_t _file = match _file with 
  | Text txt -> Text txt
  | Directory dir -> match dir with
    | [] -> Directory [] 
    | hd :: tl -> let rec parcours hd listehash= match hd,tl with
      | hd, [] -> (hd, false, Digest.string hd, Text hd) :: listehash
      | hd, tl -> parcours tl ((hd, false, Digest.string hd, Text hd) :: listehash) in
      Directory (parcours hd []);;
(* hash repo faut avoir hash de tout son contenu*)
let rec store_work_directory () = 
  let contenu = Sys.readdir "repo/" in
  let rec parcours contenu  = match contenu with 
    | [] -> Sys.command ("echo ") (*ici Execute the given shell command and return its exit code a cause de sys.command type int renvoyer car exit code *)
    | hd :: tl -> if Sys.is_directory ("repo/"^hd) then Sys.command ("cd .ogit/objects && touch " ^ hash ( parcours_t hd) ) 
      (*ici Execute the given shell command and return its exit code a cause de sys.command type int renvoyer car exit code *)

      else begin 
        let err = Sys.command ("cd .ogit/objects && touch " ^ hash ( parcours_t hd)  ^ " && echo -n \" " ^ read_text_object hd  ^ "\" > " ^ Digest.to_hex (Digest.string (read_text_object hd))) in
        if err <> 0 then failwith "erreur" else
        parcours tl
      end
    in parcours (Array.to_list contenu)
    
 

let read_directory_object _h = failwith "TODO"
  
let clean_work_directory () = Sys.command("rm -rf repo/*") (*voir message regex khalil*)

let restore_work_directory _obj = failwith "TODO" 

let merge_work_directory_I _obj = failwith "TODO"