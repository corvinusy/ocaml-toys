let rec input () =
  try
    let x = read_line () in
    x :: input ()
  with End_of_file -> [];;

let xs = input () in

let odd_printer is_odd x =
  match is_odd with
  | true -> (print_endline x; false)
  | false -> true in

List.fold_left odd_printer false xs
