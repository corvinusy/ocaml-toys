(* returns an array of n elements *)
let make_array n = (* TODO *)
  Array.to_list (Array.make n 1);;

let () =
  let n = int_of_string (read_line ()) in
  let arr = make_array n in
  List.iter ( Printf.printf "%d " ) arr
