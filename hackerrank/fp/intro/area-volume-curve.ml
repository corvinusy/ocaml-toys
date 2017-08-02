let rec pow x n =
  match n with
  | 0 -> 1.0
  | _ -> x *. (pow x (n-1))

let calc_fun a b x =
  List.fold_left (+.) 0.0 (List.map2 (fun a' b' -> a' *. (pow x b')) a b)

let range_of_list l step =
  let rec range a' b' step =
    if a' > b' then []
    else b' :: range a' (b' -. step) step in
  match l with
  | [] | [_] -> []
  | h::t::_ -> range h t step

let compute_area a b l =
  let step = 0.001 in
  let sl = range_of_list l step in
  let rec area a b sl accu =
    match sl with
    | []  -> accu
    | h::t -> (calc_fun a b h) *. step +. (area a b t accu)
  in area a b sl 0.0

let compute_vol a b l =
  let step = 0.001 in
  let sl = range_of_list l step in
  let pi = 4.0 *. atan 1.0 in
  let rec vol a b sl accu =
    match sl with
    | [] -> accu
    | h::t -> let y = calc_fun a b h in step *. pi *. y *. y +. (vol a b t accu)
  in vol a b sl 0.0

let rec int_list_of_string s =
  try
    Scanf.sscanf s "%d %[0-9-+ ]"
      (fun h t -> h :: int_list_of_string t)
  with
  | End_of_file | Scanf.Scan_failure _ -> []

let () =
  let ia = int_list_of_string (read_line()) in
  let a = List.map float_of_int ia in
  let b = int_list_of_string (read_line()) in
  let il = int_list_of_string (read_line()) in
  let l = List.map float_of_int il in
  let area = compute_area a b l in
  let vol = compute_vol a b l in
  Printf.printf "%.1f\n" area;
  Printf.printf "%.1f\n" vol
