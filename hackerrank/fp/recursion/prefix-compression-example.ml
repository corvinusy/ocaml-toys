let _ =
  let sb = Scanf.Scanning.stdin in
  let x = Scanf.bscanf sb "%s " (fun s -> s) in
  let y = Scanf.bscanf sb "%s " (fun s -> s) in
  let pos = ref 0 in
  let nx = String.length x in
  let ny = String.length y in
  let n = min nx ny in
  while !pos < n && x.[!pos] = y.[!pos] do
    incr pos
  done;
  Printf.printf "%d %s\n" !pos (String.sub x 0 !pos);
  Printf.printf "%d %s\n" (nx - !pos) (String.sub x !pos (nx - !pos));
  Printf.printf "%d %s\n" (ny - !pos) (String.sub y !pos (ny - !pos))
