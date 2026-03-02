(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 2 **)
(** Tail Recursion/HOFs **)

exception UnequalLengths

let vector_add (v1 : int list) (v2 : int list) : int list =
  let rec helper v1 v2 acc =
    match v1, v2 with
    | [], [] -> List.rev acc
    | x1::t1, x2::t2 -> helper t1 t2 ((x1 + x2) :: acc)
    | _ -> raise UnequalLengths
  in
  helper v1 v2 []

let repeat (c: char) (n: int) : char list =
  let rec helper n acc =
    if n <= 0 then acc
    else helper (n - 1) (c :: acc)
  in
  helper n []

let derive (f : float -> float) : (float -> float) = 
  let epsilon = 1e-8 in
  fun x -> (f (x +. epsilon) -. f x) /. epsilon

let for_all (f : 'a -> bool) (l : 'a list) : bool =
  List.fold_left (fun acc x -> acc && f x) true l
  
let matrix_valid (mat : int list list ) : bool = 
  match mat with
  | [] -> false
  | row::rows ->
      let len = List.length row in
      for_all (fun r -> List.length r = len) rows

let compose_all (fns : ('a -> 'a) list) : ('a -> 'a) =
  List.fold_left
    (fun acc f -> fun x -> f (acc x))
    (fun x -> x)
    fns

let run_length_decode (l: (char * int) list) : char list =
  List.fold_left
    (fun acc (c, n) -> acc @ (repeat c n))
    []
    l

let run_length_encode (l : char list) : (char * int) list =
  match l with 
  | [] -> []
  | hd::tl -> 
      let first_acc = [(hd, 1)] in 
      let final_acc = List.fold_left (fun acc next_value -> 
        match acc with
          | (c, n) :: rest ->
              if c = next_value then
                (c, n + 1) :: rest
              else
                (next_value, 1) :: acc
          | [] -> acc
      ) first_acc tl in 
      List.rev final_acc

let fold_right (f: 'a -> 'b -> 'b) (l: 'a list) (u: 'b) : 'b =
  raise Util.Unimplemented
