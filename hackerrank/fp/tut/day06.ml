let rec explode_to_even_odd s even odd = 
  let slen = String.length s in
  match slen with
  | 0 -> (List.rev even, List.rev odd)
  | 1 -> (List.rev (s.[0]::even), List.rev odd)
  | _ -> explode_to_even_odd (String.sub s 2 (slen - 2)) (s.[0]::even) (s.[1]::odd)

let rec prnt tup =
  List.iter print_char (fst tup);
  print_char ' ';
  List.iter print_char (snd tup);
  print_newline ()

let () =
  let n = int_of_string(input_line stdin) in
  for _ = n downto 1 do
    let s = input_line stdin in
    prnt (explode_to_even_odd s [] [])
  done
