module IntPairs =
  struct
    type t = int * int
      let compare (x0,y0) (x1,y1) =
        match Pervasives.compare x0 x1 with
            0 -> Pervasives.compare y0 y1
          | c -> c
  end

module PairsSet = Set.Make(IntPairs);;

let print_set s =
  PairsSet.iter (fun (x, y) -> print_int x; print_char ' '; print_int y; print_char ' ') s

let rec print_list_of_sets l =
  match l with
    [] -> ()
  | h::t -> print_set h; print_list_of_sets t

let rec list_of_pairs l1 l2 =
  match l1 with
    [] -> l2
    | _::[] -> l2
    | h1::h2::t -> (list_of_pairs t l2@[(h1, h2)] )

let rec list_of_sets n fslist =
  match n with
    0 -> fslist
    | _ -> let sl = String.split_on_char ' ' (String.trim (input_line stdin)) in
            let ints = List.map int_of_string sl in
            let pairs = list_of_pairs ints [] in
            let factors_set = (List.fold_right PairsSet.add pairs) in
            list_of_sets (n-1) fslist@[factors_set]

let () =
  let n = int_of_string (String.trim (input_line stdin)) in
  print_list_of_sets (list_of_sets n [])

