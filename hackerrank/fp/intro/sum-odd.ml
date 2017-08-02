(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec read_lines () =
    try
        let line = read_line () in
        int_of_string (line) :: read_lines()
    with End_of_file -> [];;

let plus_if_odd accu x =
    match x mod 2 with
    | 0 -> accu
    | _ -> accu + x

let sum_odds lst =
    List.fold_left plus_if_odd 0 lst

let () =
    let lst = read_lines () in
    print_int (sum_odds lst)
