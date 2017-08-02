let rec read_lines () =
  try let line = read_line () in
    int_of_string (line) :: read_lines()
  with
    End_of_file -> []
;;
let rec replicate_num num times =
  match times with
    0 -> []
  | _ -> num :: replicate_num num (times-1)
;;
let rec replicate_lst times lst =
  match lst with
    [] -> []
  | hd :: tl -> (replicate_num hd times) @ replicate_lst times tl
;;
let () =
  let n :: arr = read_lines() in
  let ans = replicate_lst n arr in
  List.iter (fun x -> print_int x; print_newline ()) ans;;
