let rec fact n =
  match n with
  | 0 | 1 -> 1
  | _ -> n * fact (n-1)

 let coeff row col =
    (fact row) / ((fact col) * (fact (row-col)))

let calc_coeffs row_lst =
  let row = List.length row_lst - 1 in
  List.mapi (fun col _ -> coeff row col) row_lst

let next_row pascal =
  match pascal with
  | [] -> [[1]]
  | a :: _ -> (calc_coeffs (a @ [1])) :: pascal

let rec make_pascal n =
  match n with
  | 0 -> [[1]]
  | _ -> next_row (make_pascal (n-1))
;;

let print_row r =
  List.iter (Printf.printf ("%d ")) r; print_newline()

let print_pascal p =
  List.iter print_row (List.rev p)
;;

let () =
  print_pascal (make_pascal (read_int()))
