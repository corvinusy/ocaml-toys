let rec read_lines () =
  try let line = read_line () in
    int_of_string (line) :: read_lines()
  with
    End_of_file -> [] ;;

let filter_odd lst =
    let rec filter_with_acc lst acc =
        match lst with
        | [] | [_] -> acc
        | _ :: [t] -> t :: acc
        | hde :: hdo :: tl -> filter_with_acc tl (hdo :: acc)
    in List.rev (filter_with_acc lst [])
;;

let () =
  let lst = read_lines() in
  let rslt = filter_odd lst in
  List.iter (fun x -> print_int x; print_newline ()) rslt;;
