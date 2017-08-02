(* let rotate_left s =
    s
    |> String.to_list
    |> (fun (head::rest) -> rest @ [head])
    |> String.of_char_list
 *)

let rotate_string s =
    match s with
    | "" -> ""
    | "_" -> "_"
    | _ -> Bytes.cat (Bytes.sub s 1 ((String.length s) - 1)) (Bytes.sub s 0 1)

let append_string accu s =
        match s with
        | "" -> []
        | _  -> accu @ [s]

let rec make_slist accu n s =
    if n > 0 then
        let s = (rotate_string s) in
        let accu = append_string accu s in
        make_slist accu (n-1) s
    else
        accu

let rec print_strings lst =
    match lst with
    | [] -> print_newline ()
    | a :: l -> print_string (a); print_char ' '; print_strings l

let rec compute n =
    if n > 0 then
        let s = read_line () in
        print_strings (make_slist [] (String.length s) s);
        compute (n-1)

let () =
    let n = read_int () in compute n
