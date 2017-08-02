open Big_int

let huge_gcd a b =
  let u = List.fold_left Big_int.mult_big_int Big_int.unit_big_int a in
  let v = List.fold_left Big_int.mult_big_int Big_int.unit_big_int b in
  let w = Big_int.gcd_big_int u v in
  Big_int.mod_big_int w (Big_int.big_int_of_int 1000000007)

let () =
  let _ = input_line stdin in
  let al = String.split_on_char ' ' (String.trim (input_line stdin)) in
  let a = List.map Big_int.big_int_of_string al in
  let _ = input_line stdin in
  let bl = String.split_on_char ' ' (String.trim (input_line stdin)) in
  let b = List.map Big_int.big_int_of_string bl in
  print_endline (Big_int.string_of_big_int (huge_gcd a b))
