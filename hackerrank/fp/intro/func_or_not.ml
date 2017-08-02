let () =
  let s = input_line stdin in
  print_string s;
  let q = int_of_string (String.trim s) in
  print_int q;
  for _ = q downto 1 do
    begin
      let n = int_of_string(input_line stdin) in
      let tbl = Hashtbl.create n in
      let ok = ref true in
      for _ = n downto 1 do
        let l = String.split_on_char ' ' (input_line stdin) in
        if !ok then
          begin
            let x = int_of_string(List.nth l 0) in
            let y = int_of_string(List.nth l 1) in
            if not (Hashtbl.mem tbl x) then
              Hashtbl.add tbl x y
            else
            if y != Hashtbl.find tbl x then
              ok := false
          end
      done;
      if !ok then
        print_endline "YES"
      else
        print_endline "NO"
    end
  done
