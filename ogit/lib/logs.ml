type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
    
let date_fm _d =
    let d = Unix.localtime _d in
    Printf.sprintf "%02d:%02d:%02d-%02d/%02d/%04d" d.Unix.tm_hour d.Unix.tm_min d.Unix.tm_sec (d.Unix.tm_mday) (d.Unix.tm_mon+1) (d.Unix.tm_year+1900)

let set_head _l = (*Digest.t list -> unit*)
    let oc = open_out "main/repo/.ogit/HEAD" in
    List.iter (fun x -> Printf.fprintf oc "%s " (Digest.to_hex x)) _l;
    close_out oc

let get_head () =
    let ic = open_in "main/repo/.ogit/HEAD" in
    let l = ref [] in
    try
        while true do
            l := (Digest.from_hex (input_line ic))::!l
        done;
        !l
    with End_of_file -> close_in ic; !l 

let make_commit _s  _h = (*string -> Digest.t -> commit*)
    let d = Unix.time () in
    let c = Digest.to_hex(Digest.string _s) in
    let p = _h in
    {parents = p; date = d; message = _s; content = c}

let init_commit () = (*unit -> commit*)
    let d = Unix.time () in
    let c = Digest.to_hex(Digest.string "") in
    let p = [] in
    {parents = p; date = d; message = ""; content = c}

let store_commit _c = (*commit -> unit*)
    let oc = open_out ("main/repo/.ogit/logs/" ^ (Digest.to_hex _c.content)) in
    Printf.fprintf oc "%s\n" (date_fm _c.date);
    Printf.fprintf oc "%s\n" _c.message;
    List.iter (fun x -> Printf.fprintf oc "%s " (Digest.to_hex x)) _c.parents; 
    (*List.iter = List.map mais en appliquant directement sur la fonction en paramètre et return [] aulieu de return une new list*)
    Printf.fprintf oc "\n";
    close_out oc

(*A finir car ya des erreurs*)
let read_commit _h = (*Digest.t -> commit*)
    let ic = open_in ("main/repo/.ogit/logs/" ^ (Digest.to_hex _h)) in
    let l = ref [] in
        try
            while true do
                l := (input_line ic)::!l
            done;
            !l
        with End_of_file -> close_in ic;
    let t = !l in 
    let d = t.(1) in (*Lit la 2ème ligne du fichier log qui correspond à la date*)
    let filecommit = open_in ("main/repo/.ogit/objects/" ^ l.(3)) in (*Lit la 4ème ligne du fichier log qui correspond au hash du commit*)
    let m = ref [] in
        try
            while true do
                m := (input_line filecommit)::!m
            done;
            !m
        with End_of_file -> close_in filecommit; !m
    let p = ref [] in
    try
        while true do
            p := (Digest.from_hex (input_line ic))::!p
        done;
    with End_of_file -> close_in ic; {parents = !p; date = d; message = m; content = _h}