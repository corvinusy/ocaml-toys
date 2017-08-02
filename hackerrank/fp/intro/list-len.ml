(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec read_lines () =
    try
        let line = read_line () in
        int_of_string (line) :: read_lines()
    with End_of_file -> [];;

let rec sizeof_accu accu lst =
    match lst with
    | [] -> accu
    | a::l -> sizeof_accu (accu + 1) l

let sizeof lst =
    sizeof_accu 0 lst
;;

let () =
    let lst = read_lines () in
    print_int (sizeof lst)
