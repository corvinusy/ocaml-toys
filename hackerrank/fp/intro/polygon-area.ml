let distance p1 p2 =
  let dx = (fst p2) - (fst p1) in
  let dy = (snd p2) - (snd p1) in
  sqrt(float_of_int(dx * dx + dy * dy))


let triangle_area p1 p2 p3 =
   let p = ((distance p1 p2) +. (distance p2 p3) +. (distance p1 p3)) /. 2.0 in
   let a = (distance p1 p2) in
   let b = (distance p1 p3) in
   let c = (distance p2 p3) in
   sqrt(p *. (p -. a) *. (p -. b) *. (p -. c))

let area p =
  let rec aux points p0 accu =
    match points with
    | [] | [_] -> accu
    | p1::p2::tail -> aux (p2::tail) p0 (accu +. (triangle_area p0 p1 p2))
  in
    match p with
    | [] -> 0.0
    | hd::tl -> aux tl hd 0.0

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
  Printf.printf "%.1f" (area points)
