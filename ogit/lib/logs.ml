type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}

let put_file_in_list ic =
    let l = ref [] in
    try
        while true do
            l := (input_line ic)::!l
        done;
        !l
    with End_of_file -> close_in ic; !l

let date_fm _d =
    let d = Unix.localtime _d in
    Printf.sprintf "%02d:%02d:%02d-%02d/%02d/%04d" d.Unix.tm_hour d.Unix.tm_min d.Unix.tm_sec (d.Unix.tm_mday) (d.Unix.tm_mon+1) (d.Unix.tm_year+1900)

let set_head _l =
    let oc = open_out ".ogit/HEAD" in
    List.iter (fun x -> Printf.fprintf oc "%s\n" (Digest.to_hex x)) _l;
    close_out oc

let get_head () =
    let ic = open_in ".ogit/HEAD" in
    let l = ref [] in
    try
        while true do
            l := (Digest.from_hex (input_line ic))::!l
        done;
        !l
    with End_of_file -> close_in ic; List.rev !l 

let make_commit _s _h =
    let head = get_head () in
    let objfile = open_in (".ogit/objects/" ^ _h) in
    let d = Unix.time () in
    let c = Digest.string (String.trim(String.concat "\n" (put_file_in_list objfile))) in
    {parents = head; date = d; message = _s; content = c}


(*A finir quand Objects.store_work_directory sera fini 
voir  avec lozes ou marie comment on doit faire    
*)
let init_commit () =
    let d = Unix.time () in
    let c = Digest.to_hex(Digest.string "") in
    let p = [] in
    {parents = p; date = d; message = "init commit"; content = c (*Objects.store_work_directory*)}

let store_commit _c =
    let oc = open_out (".ogit/logs/" ^ (Digest.to_hex _c.content)) in
    (*List.iter = List.map mais en appliquant directement sur la fonction en paramètre et return [] aulieu de return une new list*)
    (*Important car le typage de la fonction n'est pas bon sans le List.iter*)
    List.iter (fun x -> Printf.fprintf oc "%s\n" (Digest.to_hex x)) _c.parents;
    Printf.fprintf oc "%s\n" (date_fm _c.date);
    Printf.fprintf oc "%s\n" _c.message;
    Printf.fprintf oc "%s" (Digest.to_hex _c.content);
    close_out oc;
    _c.content

let convert_date_fm_to_timestamp _s =
    let l = String.split_on_char '-' _s in
    let hour = String.split_on_char ':' (List.nth l 0) in
    let date = String.split_on_char '/' (List.nth l 1) in
    let d = fst (Unix.mktime { 
        Unix.tm_hour = int_of_string (List.nth hour 0); 
        Unix.tm_min = int_of_string (List.nth hour 1); 
        Unix.tm_sec = int_of_string (List.nth hour 2);
        Unix.tm_mday = int_of_string (List.nth date 0); 
        Unix.tm_mon = int_of_string (List.nth date 1) - 1; 
        Unix.tm_year = int_of_string (List.nth date 2) - 1900;
        Unix.tm_wday = 0; Unix.tm_yday = 0; Unix.tm_isdst = false
    }) in d

let read_commit _h =
let ic = open_in (".ogit/logs/" ^ (Digest.to_hex _h)) in
let l = put_file_in_list ic in
let c = Digest.from_hex (List.nth l 0) in
let m = List.nth l 1 in
let d = convert_date_fm_to_timestamp (List.nth l 2) in
let p = ref (List.rev((List.init (List.length l - 3) (fun x -> Digest.from_hex (List.nth l (x+3)))))) in
{parents = !p; date = d; message = m; content = c}

(*Demander Lozes si utile
let clear_logs ()= let err=Sys.command "rm -rf main/.ogit/logs/* && rm -rf main/.ogit/HEAD" in 
if err<>0 then failwith "clear_logs failed";;*)