(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 1 **)
(** Warmup **)

let rec sum (n: int) : int =
  if n = 0 then n
  else n + sum (n - 1)

let rec sumsq (n: int) : int =
  if n = 0 then n
  else (n * n) + sumsq (n - 1)

let rec sumOdd (n: int) : int =
  if n = 0 then 0
  else ((n * 2) - 1) + sumOdd (n - 1)

let rec fib (n: int) : int =
  if n <= 1 then 1
  else fib (n - 1) + fib (n - 2)

let fibFast n =
  let rec helper (k: int) (prev_1: int) (prev_2: int) : int =
    if k = n then prev_2
    else helper (k + 1) prev_2 (prev_2 + prev_1) in
  helper 2 0 1

let sinappx (n: int) (x : float) : float =
  let rec helper (k: int) (i: float) (x: float) (factorial: float) : float = 
    if k = n then ((((-1.0) ** i) /. factorial) *. (x ** (2.0 *. i +. 1.0))) 
    else ((((-1.0) ** i) /. factorial) *. (x ** (2.0 *. i +. 1.0)))  +. helper (k + 1) (i +. 1.0) x (factorial *. (2.0 *. i +. 2.0) *. (2.0 *. i +. 3.0)) in
    helper 0 0.0 x 1.0

let rec repeat (c: char) (n: int) : char list =
  match n with
  | 0 -> []
  | _ -> c :: repeat c (n - 1)

let rec run_length_encode (l : char list) : (char * int) list =
  let rec helper (c: char) (count: int) (helper_l: char list) : (char * int) list = 
    match helper_l with
    | [] -> (c, count) :: run_length_encode helper_l
    | h :: t -> if h = c then helper c (count + 1) t else (c, count) :: run_length_encode helper_l in

  match l with
  | [] -> []
  | h :: t -> helper h 1 t


let rec run_length_decode (l: (char * int) list) : char list =
  match l with
  | [] -> []
  | h :: t -> repeat (fst h) (snd h) @ run_length_decode t
