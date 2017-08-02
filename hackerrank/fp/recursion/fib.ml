let rec fib n =
    match n with
    | 1 -> 0
    | 2 -> 1
    | _ -> fib (n-1) + fib (n-2)

let () =
    print_int (fib (read_int()))
