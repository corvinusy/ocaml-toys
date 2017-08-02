let rec sum_of_pow x n accu =
  let y = x - (int_of_float ((float_of_int accu) ** (float_of_int n))) in
  match y with
  | 0 -> 1
  | y when y < 0 -> 0
  | _ -> (sum_of_pow y n (accu + 1)) + (sum_of_pow x n (accu + 1))

let () =
  let x = int_of_string (read_line()) in
  let n = int_of_string (read_line()) in
  print_int (sum_of_pow x n 1)
