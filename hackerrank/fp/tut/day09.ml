let rec factorial n =
  match n with
  | 0 -> n
  | 1 -> n
  | _ -> n * factorial (n-1)

let () =
  print_int (factorial (int_of_string(input_line stdin)))
