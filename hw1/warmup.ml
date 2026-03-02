(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 1 **)
(** Warmup **)

let rec sum (n: int) : int =
  if n > 0 then n + sum (n - 1) else 0

let rec sumsq (n: int) : int =
  if n > 0 then (n * n) + sumsq (n - 1) else 0

let rec sumOdd (n: int) : int =
  n * n

let rec fib (n: int) : int =
  if n <= 1 then 1
  else fib (n - 1) + fib (n - 2)

let fibFast n =
  let rec fibhelper n a b = 
    if n = 0 then a
    else fibhelper (n - 1) b (a + b) in
  fibhelper n 1 1

let sinappx (n: int) (x : float) : float =
  let rec sinhelper (k: int) (i: float) (sign: float) (fac: float) : float =
    if k = n then 0.
    else
      ((x ** (2. *. i +. 1.)) /. fac  *. sign) +. sinhelper (k + 1) (i +. 1.) (sign *. (-1.)) ( (2. *. i +. 2.) *. (2. *. i +. 3.) *. fac ) in
    sinhelper 0 0. 1. 1.

let rec repeat (c: char) (n: int) : char list =
  if n <= 0 then []
  else c :: repeat c (n - 1)

let rec run_length_encode (l : char list): (char * int) list=
  if l = [] then []
  else let rec helper_encode (helper_l: char list) (a: char) (k: int) =
    match helper_l with
    | h :: t when h = a -> helper_encode t a (k + 1)
    | _ -> (a, k) :: run_length_encode helper_l in
  match l with
  | [] -> []
  | h :: t -> helper_encode l h 0

let rec run_length_decode (l: (char * int) list) : char list =
  
  let rec repeat_modified (c: char) (n: int) (lst: (char * int) list): char list =
  if n <= 0 then run_length_decode lst
  else c :: repeat_modified c (n - 1) lst in

    match l with
    | [] -> []
    | h :: t -> repeat_modified (fst h) (snd h) t
