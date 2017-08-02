let tauro_sum ar x y =
  ar.(x).(y) + ar.(x).(y+1) + ar.(x).(y+2) +
  ar.(x+1).(y+1) +
  ar.(x+2).(y) + ar.(x+2).(y+1) + ar.(x+2).(y+2)


let max_tauro ar =
  let rec loop ar x y accu =
    let t = tauro_sum ar x y in
    match (x, y) with
    | (3, 3) -> max accu t
    | (x, 3) -> loop ar (x+1) 0 (max accu t)
    | (x, y) -> loop ar x (y+1) (max accu t)
  in
  loop ar 0 0 min_int


let () =
  let ar = Array.make_matrix 6 6 0 in
  for i = 0 to 5 do
    let sl = String.split_on_char ' ' (String.trim (input_line stdin)) in
    let l = List.map int_of_string sl in

    ar.(i) <- Array.of_list l
  done;
  print_int (max_tauro ar)
