let meal = read_float ()
let tip = read_float ()
let tax = read_float () 
let totalCost ~meal ~tip ~tax =
  meal +. meal *. tip *. 0.01 +. meal *. tax *. 0.01;;
Printf.printf "The total meal cost is %d dollars." (int_of_float (totalCost +. 0.5))
