let segment_area p1 p2 =
  let dx = (fst p2) - (fst p1) in
  let ay = float_of_int ((snd p2) + (snd p1)) /. 2.0 in
  float_of_int (dx) *. ay

let full_area p =
  let rec aux points accu =
    match points with
    | [] | [_] -> accu
    | p1::p2::tail -> aux (p2::tail) (accu +. (segment_area p1 p2))
  in
    (* add the first point to the list of points *)
    aux (p @ [List.hd p]) 0.0


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
  Printf.printf "%.1f" (full_area points)
