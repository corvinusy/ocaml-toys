let freq_tbl a =
  let tbl = Hashtbl.create (List.length a) in
  let rec aux tbl a =
    match a with
    | [] -> tbl
    | h :: t ->
      if Hashtbl.mem tbl h then
        Hashtbl.replace tbl h ((Hashtbl.find tbl h)+1)
      else
        Hashtbl.add tbl h 1;
      aux tbl t
  in aux tbl a

let filter_list k a =
  let tbl = freq_tbl a in
  let rec aux k a accu_list =
    match a with
    | [] -> List.rev accu_list
    | h::t ->
      if List.mem h accu_list then
        aux k t accu_list
      else
      if (Hashtbl.find tbl h) >= k then
        aux k t (h::accu_list)
      else
        aux k t accu_list
  in
  aux k a []

let read_eval_print() =
  (* read input *)
  let nk = read_line () in
  let (n, k) = Scanf.sscanf nk "%d %d" (fun x y -> (x, y)) in
  let int_list = read_line ()
                 |> String.trim
                 |> String.split_on_char ' '
                 |> List.map int_of_string in
  (* evaluate *)
  let filtered_list = filter_list k int_list in
  (* print *)
  if (List.length filtered_list) = 0 then
    print_string "-1"
  else
    List.iter  (fun x -> Printf.printf "%d " x) filtered_list;
  print_newline()

let () =
  let q = read_int () in
  for _ = 1 to q do
    read_eval_print()
  done
