let int_of_numchar c = (int_of_char c) - (int_of_char '0')

let sd_loop_inner sn =
  let rec aux s accu =
    match String.length s with
    | 0 -> accu
    | 1 -> accu + (int_of_string s)
    | n ->
      let str_head = String.sub s 0 (n-1) in
      aux str_head (accu + int_of_numchar s.[n-1])
  in aux sn 0

let rec sd_loop s =
  let x = sd_loop_inner s in
  match x with
  | x when x <= 9 -> x
  | _ -> sd_loop (string_of_int x)

let superdigit sn k =
  match k with
  | 0 -> 0
  | _ -> sd_loop (string_of_int (k * (sd_loop_inner sn)))

let () =
  let slist = read_line ()
              |> String.trim
              |> String.split_on_char ' '
  in match slist with
  | sn :: sk :: _ ->  print_int (superdigit sn (int_of_string sk))
  | _ -> ()
