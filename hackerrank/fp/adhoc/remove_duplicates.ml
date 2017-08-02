
let c_index x = (int_of_char x) - (int_of_char 'a')

let () =
  let s = String.trim (input_line stdin) in
  let a = Array.make 26 true in
  String.iter (fun x -> if a.(c_index x) then print_char x; a.(c_index x) <- false ) s
