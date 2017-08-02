let serp_init n =
  let rec loop i lst =
    if i < n-1 then
      match lst with
      | [] -> lst
      | h::t -> loop (i+1) ( ([1] @ h @ [1]) :: lst )
    else
      List.rev lst
  in
  loop 0 [[1]]

let rec fill_zeroes lst n =
  if List.length lst >= n then
    lst
  else
    fill_zeroes ([0]@lst@[0]) n

let serpinsky lst = fill_zeroes lst 63

let serpinsky_iter lst =
  let rlst = List.rev lst in
  let rec aux a b =
    List.map2 (lxor) (List.hd lst) (List.hd rlst) in
  aux lst rlst


let rec serp_print lst =
  match lst with
  | [] -> ()
  | h :: t -> List.iter print_int (serpinsky h); print_newline (); serp_print t

let () =
  let n = read_int () in
  serp_print (serp_init 32)
