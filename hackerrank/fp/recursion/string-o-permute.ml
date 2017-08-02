let opermute s =
    let rec aux s i accu =
        if i <= 0 then
            accu
        else
            aux s (i-2) (s.[i-1]::s.[i]::accu)
    in aux s (String.length s - 1) []

let rec compute t =
    match t with
    | 0 -> ()
    | _ -> List.iter print_char (opermute (read_line())); compute (t-1)

let () =
    let t = read_int() in compute t
