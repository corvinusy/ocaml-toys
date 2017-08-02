let lcm_big a b =
  let u = Big_int.mult_big_int a b in
  let v = Big_int.gcd_big_int a b in
  Big_int.div_big_int u v

let () =
  let _ = int_of_string (String.trim (input_line stdin)) in
  let l = String.split_on_char ' ' (String.trim (input_line stdin)) in
  let a = List.map Big_int.big_int_of_string l in
  print_string (Big_int.string_of_big_int (List.fold_left lcm_big Big_int.unit_big_int a))
