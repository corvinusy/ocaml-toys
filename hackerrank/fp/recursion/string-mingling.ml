let mingle a b =
    let rec aux a b i accu =
        match i with
        | -1 -> accu
        | _ -> aux a b (i-1) (a.[i]::b.[i]::accu) in
    aux a b (String.length a-1) []

let () =
    let a = read_line() in
    let b = read_line() in
    List.iter print_char (mingle a b)
