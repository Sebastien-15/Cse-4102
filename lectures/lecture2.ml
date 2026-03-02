let x = 1 in (x + x) * (let x = 2 in x + x);;
let x = 1 in (x * 2) + (let x = 2 in x + 5) + 4 * x;;
let x : int = (let y = 3 in y * 2) + 5 in (let y = 1 in x + y);;
let z = (1. : float) in z +. 2.5;;

let f (x: int) : int = x + 1;;
let five = f 4;;
let five' = f 4;;

let my_plus (x: int) (y : int) : int = x + y;;

let my_plus = fun x -> fun y -> x + y

let seven = my_plus 3 4

let six = (fun (x : int) -> x + 1) 5;;

let f' = fun (x : int) -> x + 1

(* Factorial: return n!
 * n : int (* Hi! Nested comment! *)
*)
let rec fact (n : int) : int =
    if n <= 0 then 1
    else n * (fact (n - 1))

let _ = Printf.printf "fact(%d) = %d\n" 0 (fact 0)
let _ = Printf.printf "fact(%d) = %d\n" 5 (fact 5)

let _ = assert (fact 0 = 1)
let _ = assert (fact 5 = 120)

let rec fib (n : int) : int =
    if n <= 1 then 1
    else fib (n - 2) + fib (n - 1)

let _ = assert (fib 0 = 1)
let _ = assert (fib 1 = 1)
let _ = assert (fib 2 = 2)
let _ = assert (fib 3 = 3)
let _ = assert (fib 4 = 5)

let plus_one = my_plus 1;; (* fun y -> 1 + y *)
let plus_one = (+) 1;;
