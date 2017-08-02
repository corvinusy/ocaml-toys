let count_ones n =
  let rec loop n c m =
    match (n, n mod 2) with
    | (0, _) -> if c > m then c else m
    | (_, 0) -> if c > m then loop (n lsr 1) 0 c else loop (n lsr 1) 0 m
    | (_, _) -> loop (n lsr 1) (c+1) m
  in
  loop n 0 0


let () =
  let n = int_of_string (String.trim (input_line stdin)) in
  print_int (count_ones n)
