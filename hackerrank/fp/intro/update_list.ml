let f arr =
    let rec apply fu lst =
        match lst with
        [] -> []
        | a::l -> (fu a) :: apply fu l in
    apply abs arr

let rec read_lines () =
    try let line = read_line () in
        line :: read_lines()
    with
        End_of_file -> []

let () =
    let inp = read_lines () in
    let arr = List.map int_of_string inp in
    let result = f arr in
    let output = List.map string_of_int result in
    print_string (String.concat "\n" output) ;;
