let rec input () =
    try
        let x = read_line () in
        x :: input ()
    with End_of_file -> [];;

let rec append_rev lst acc =
    match lst with
    | [] -> acc
    | hd :: tl -> append_rev tl (hd :: acc )

let () =
    let lst = input () in
    List.iter print_endline (append_rev lst []);;
