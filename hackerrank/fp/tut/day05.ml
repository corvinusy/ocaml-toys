let rec mul_printer num x =
    Printf.printf ("%d x %d = %d\n") num x (num * x); 
    if x > 0 && x < 10 then mul_printer num (x+1)
in mul_printer (read_int()) 1
