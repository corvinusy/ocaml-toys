let distance p1 p2 =
  let dx = (fst p2) - (fst p1) in
  let dy = (snd p2) - (snd p1) in
  sqrt(float_of_int(dx * dx + dy * dy))

let perimeter p =
  let rec aux p accu =
    match p with
    | [] | [_] -> accu
    | h1::h2::t -> aux (h2::t) (accu +. (distance h1 h2))
  in
  aux p 0.0

let () =
  let n = int_of_string (String.trim (input_line stdin)) in
  let rec aux i p =
    match i with
    | 0 -> p
    | _ ->
      begin
        let l = String.split_on_char ' ' (String.trim (input_line stdin)) in
        let x = int_of_string (List.nth l 0) in
        let y = int_of_string (List.nth l 1) in
        aux (i-1) p @ [(x, y)]
      end
  in
  let points = aux n [] in
  print_float (perimeter (points @ [List.hd points]))
