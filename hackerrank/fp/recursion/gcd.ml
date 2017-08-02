let rec gcd a b =
    match b mod a with
    | 0 -> a
    | _ -> gcd (b mod a) a

let () =
    let input = read_line() in
    print_int (Scanf.sscanf input "%d %d" gcd)
