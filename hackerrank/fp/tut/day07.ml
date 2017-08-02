let prn s =
  print_string s; print_char ' '

let () =
  let _ = input_line stdin in
  let l = String.split_on_char ' ' (input_line stdin) in
  List.iter prn (List.rev l)
