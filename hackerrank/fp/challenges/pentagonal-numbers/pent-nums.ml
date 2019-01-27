(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let () =
  let t = int_of_string (read_line () ) in
  for _ = t downto 1 do
    let n = int_of_string (read_line () ) in
    print_int( (3 * n * n - n) / 2 ); print_newline()
   done
