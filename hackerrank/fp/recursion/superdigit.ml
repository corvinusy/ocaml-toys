let sum_of_digits s =
  let rec aux s i accu =
    match i with
    | 0 -> accu
    | _ ->
      let a = accu - (int_of_char '0') + (int_of_char s.[i-1]) in
      aux s (i-1) a
  in
  aux s (String.length s) 0

let rec superdigit s k =
  match k * (String.length s) with
  | 1 -> s
  | _ ->
    let new_s = string_of_int (k * (sum_of_digits s))
    in superdigit new_s 1

let () =
  let slist = read_line ()
              |> String.trim
              |> String.split_on_char ' '
  in match slist with
  | n_str :: k_str :: _ ->  let k = int_of_string k_str in print_string (superdigit n_str k)
  | _ -> ()
