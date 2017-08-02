let memoize f =
  let table = Hashtbl.create 1000 in
  (fun x ->
     if Hashtbl.mem table x then
       Hashtbl.find table x
     else
       let y = f x in
       Hashtbl.add table x y;
       y
  );;

let memo_rec f_norec x =
  let fref = ref (fun _ -> assert false) in
  let f = memoize (fun x -> f_norec !fref x) in
  fref := f;
  f x

let levenshtein = memo_rec (fun levenshtein (s, t) ->
    let rec dist i j = match (i,j) with
      | (i,0) -> i
      | (0,j) -> j
      | (i,j) ->
        if s.[i-1] = t.[j-1] then dist (i-1) (j-1)
        else let d1, d2, d3 = dist (i-1) j, dist i (j-1), dist (i-1) (j-1) in
          1 + min d1 (min d2 d3)
    in
    dist (String.length s) (String.length t))

let test s t =
  Printf.printf " %s -> %s = %d\n" s t (levenshtein (s, t))

let () =
  test "ocaml" "OCaml";
  test "kitten" "sitting";
  test "rosettacode" "raisethysword";
