let rec print_hello_world n =
    match n with
        0 -> 0
    | _ -> print_endline "Hello World"; print_hello_world (n-1)

in print_hello_world (read_int())
