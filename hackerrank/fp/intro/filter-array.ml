let rec read_lines () =
  try let line = read_line () in
    int_of_string (line) :: read_lines()
  with
    End_of_file -> []
;;

let rec filter_list delim lst =
    match lst with
    | [] -> []
    | hd :: tl when hd >= delim -> filter_list delim tl
    | hd :: tl -> hd :: filter_list delim tl
;;

let () =
  let input = read_lines() in
  let ans = filter_list (List.hd input) (List.tl input)  in
  List.iter (fun x -> print_int x; print_newline ()) ans;;
