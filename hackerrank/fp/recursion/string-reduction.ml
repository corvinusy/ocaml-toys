let rec reduce s l i =
  if i > (String.length s - 1) then
    print_newline ()
  else
  if (List.exists (fun x -> s.[i] == x ) l) then
    reduce s l (i+1)
  else
    begin
      print_char s.[i];
      reduce s (s.[i]::l) (i+1)
    end;;

let () =
  let a = read_line () in
  reduce a [] 0
