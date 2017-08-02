(* let car_of_string s =
   let rec aux i l =
    if i < 0 then l else aux (i - 1) (s.[i] :: l) in
   aux (String.length s - 1) [];;

   let car_of_int n = car_of_string (string_of_int n);;

   let compress s =
   let rec aux i c num accu =
    if i < 0  then
      match num with
      | 0 -> accu
      | 1 -> (c :: accu)
      | _ -> ((c::(car_of_int num)) @ accu)
    else
    if s.[i] == c then
      aux (i-1) c (num+1) accu
    else
      match num with
      | 0 -> aux (i-1) s.[i] 1 accu
      | 1 -> aux (i-1) s.[i] 1 (c :: accu)
      | _ -> aux (i-1) s.[i] 1 ((c::(car_of_int num)) @ accu) in
   aux (String.length s-1) '_' 0 [] *)

let rec compress s i cnt =
  if i > String.length s then () 
  else if i == String.length s || s.[i] != s.[i - 1] then begin
    print_char s.[i - 1];
    if cnt > 1 then print_int cnt;
    compress s (i + 1) 1
  end
  else
    compress s (i + 1) (cnt + 1)

let () =
  let s = read_line() in
  compress s 1 1
