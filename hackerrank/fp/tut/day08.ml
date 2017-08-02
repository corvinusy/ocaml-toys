let print_phone tbl key =
  if Hashtbl.mem tbl key then
    Printf.printf "%s=%s\n" key (Hashtbl.find tbl key)
  else
    print_endline "Not Found"

let () =
  let n = int_of_string(input_line stdin) in
  let tbl = Hashtbl.create n in
  for _ = n downto 1 do
    let l = String.split_on_char ' ' (input_line stdin) in
    let name = List.hd l in
    let phone = List.hd (List.tl l) in
    Hashtbl.add tbl name phone
  done;

  try
    while true do
      print_phone tbl (input_line stdin)
    done
  with End_of_file -> ()
