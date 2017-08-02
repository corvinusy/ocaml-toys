let print_char_list l =
  print_int (List.length l);
  print_char ' ';
  List.iter print_char l;
  print_newline()

let rec fold_left2 f (accu, ok) l1 l2 =
  match (l1, l2) with
  | ([], []) -> accu
  | (h1::t1, h2::t2) -> fold_left2 f (f (accu, ok) h1 h2) t1 t2
  | (_, _) -> accu

let prefix x y =
  let n = fold_left2 (fun (accu, ok) a b -> if ok && a == b then (accu + 1, true) else (accu, false)) (0, true) x y in
  let rec loop n p l1 l2 =
    match n with
    | -1 -> [p, l1, l2]
    | _ -> match (l1, l2) with
      | ([], []) -> [p, l1, l2]
      | (h1::t1, h2::t2) -> loop (n-1) (h1::p) t1 t2
      | (_, _) -> [p, l1, l2]
  in loop (n-1) [] x y

let print_prefix [p, l1, l2] =
  print_char_list (List.rev p);
  print_char_list l1;
  print_char_list l2

let explode s =
  let rec loop i l =
    match i with
    | -1 -> l
    | _ -> loop (i - 1) (s.[i] :: l)
  in loop (String.length s - 1) []

let () =
  let x = explode (read_line()) in
  let y = explode (read_line()) in
  print_prefix (prefix x y)
