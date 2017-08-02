let weird_printer = function
  | n when (n mod 2) = 1 -> print_endline "Weird"
  | n when n <= 5 -> print_endline "Not Weird"
  | n when n <= 20 -> print_endline "Weird"
  | _ -> print_endline "Not Weird"
in weird_printer (read_int ())
