(* e^^x = 1 + 1*x/1 + x*x/2! + x*x*x/3! +...  *)
let rec fact x =
    match x with
    | 0 | 1 -> 1
    | n -> n * (fact (n-1))

let rec pow x p =
    match p with
    | 0 -> 1.
    | 1 -> x
    | n -> x *. pow x (n - 1)

let rec epow x step =
    match step with
    | 0 -> 1.
    | n -> (pow x n) /. (float_of_int (fact n)) +. (epow x (step-1))

let rec compute n =
    if n > 0 then
        let x = read_float () in
        Printf.printf ("%.4f\n") (epow x 9);
        compute (n - 1)

  let () =
    let n = read_int() in compute n
