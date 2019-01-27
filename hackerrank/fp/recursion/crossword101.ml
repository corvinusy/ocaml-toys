let read_board =
  let rec aux t accu =
    match t with
    | 0 -> List.rev accu
    | _ -> let s = read_line () |> String.trim in aux (t-1) (s::accu)
  in aux 10 []

let read_words =
  read_line() |> String.trim |> String.split_on_char ';'

let print_board b =
  List.iter print_endline b

let is_filled b =
  let rec aux lst c  =
    match lst with
    | [] -> true
    | hd::tl ->
      if String.contains hd c then false
      else aux tl c
  in aux b '-'

let place_word (b, w) = (b, w)

let fill_board b w =
  let rec aux (b, w) =
    match w with
    | [] -> b
    | hd::tl -> aux (place_word (b, tl))
  in aux (b, w)

(* next_permutation block *)
let rec find_key left right =
  match left with
  | [] | [_] -> [], None, []
  | a::b::tail ->
    if (b < a) then  (* key found *)
      List.rev tail, Some b, a::right  (* return left, key, right *)
    else
      find_key (b::tail) (a::right)

let rec swap_and_rev key left right =
  match right with
  | [] -> []
  | [a] -> a :: List.rev (left @ [key])
  | a::b::tail ->
    if (a > key) && not (b > key) then
      a :: List.rev (left @ key::b::tail)
    else
      swap_and_rev key (left @ [a]) (b::tail)

let next_permutation lst =
  let left, key, right = find_key (List.rev lst) [] in
  match key with
  | None -> []
  | Some key -> left @ swap_and_rev key [] right
(* end of next_permutation block *)

let eval b w =
  let rec aux b w =
    match is_filled b with
    | true -> b
    | false ->
      if is_filled (fill_board b w) then b
      else aux b (next_permutation w)
  in aux b w

let() =
  let board = read_board in
  let words = read_words in
  print_board board;
  let res = eval board (List.sort String.compare words) in
  print_board res
