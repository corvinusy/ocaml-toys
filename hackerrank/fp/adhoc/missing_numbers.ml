module IntSet = Set.Make(struct type t = int let compare = compare end)

let () =
  let _ = input_line stdin in
  let al = String.split_on_char ' ' (String.trim (input_line stdin)) in
  let a = IntSet.of_list (List.map int_of_string al) in
  let _ = input_line stdin in
  let bl = String.split_on_char ' ' (String.trim (input_line stdin)) in
  let b = IntSet.of_list (List.map int_of_string bl) in
  IntSet.iter print_int (IntSet.union a b); print_newline ()
