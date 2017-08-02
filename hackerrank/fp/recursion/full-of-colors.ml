let check_list a b l =
  let fold = List.fold_left (fun acc x ->
      if abs(acc) > 1 then acc
      else
      if x = a then acc + 1
      else if x = b then acc - 1
      else acc) 0 l
  in
  abs(fold) < 2 && fold == 0

let explode s =
  let rec loop i l =
    match i with
    | -1 -> l
    | _ -> loop (i - 1) (s.[i] :: l)
  in loop (String.length s - 1) []

let is_full_of_colors s =
  let l = explode s in
  check_list 'R' 'G' l && check_list 'Y' 'B' l

let print_bool b =
  match b with
  | true -> print_endline "True"
  | false -> print_endline "False";;

let () =
  let t = read_int () in
  let rec inout t =
    match t with
    | 0 -> ()
    | _ -> print_bool(is_full_of_colors (read_line())); inout (t-1)
  in
  inout t
