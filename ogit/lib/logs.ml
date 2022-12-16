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
    let newlist = List.map (fun x -> Digest.to_hex x) _l in
    let result = String.concat ";" newlist in
    begin
        output_string oc (String.trim result);
        close_out oc
    end

let get_head () =
    let ic = open_in ".ogit/HEAD" in
    let l = put_file_in_list ic in
    let result = List.map (fun x -> Digest.from_hex x) (String.split_on_char ';' (List.nth l 0)) in
    result

let check_head ()=
    let head = get_head () in match head with
    | [] -> [Digest.string ""]
    | _ -> head

let make_commit _s _h =
    let head = check_head () in
    let d = Unix.time () in
    let c = Objects.store_work_directory () in
    {parents = head; date = d; message = _s; content = c}

let init_commit () =
    let d = Unix.time () in
    {parents = [Digest.string ""]; date = d; message = "init commit"; content = Objects.store_work_directory ()}

let store_commit _c =
    let temp = List.map (fun x -> Digest.to_hex x) _c.parents in
    let result = [String.concat ";" temp;date_fm _c.date;_c.message;Digest.to_hex _c.content] in
    let name = Digest.to_hex(Digest.string ((String.trim (String.concat "\n" result)))) in
    let err = Sys.command ("printf \"" ^ (String.trim (String.concat "\n" result)) ^ "\" > .ogit/logs/" ^ name) in
    if err = 0 then Digest.from_hex name else "error"
    
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
let p = ref (List.map (fun x -> Digest.from_hex x) (String.split_on_char ';' (List.nth l 3))) in
{parents = !p; date = d; message = m; content = c}
