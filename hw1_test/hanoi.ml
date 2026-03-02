
(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 1 **)
(** Towers of Hanoi **)

exception InvalidMove

(* Some helper functions for debugging and outputting results *)

let print_hanoi (p1, p2, p3) =
  Printf.printf "->%s\n->%s\n->%s\n\n"
    (String.concat "" (List.rev (List.map string_of_int p1)))
    (String.concat "" (List.rev (List.map string_of_int p2)))
    (String.concat "" (List.rev (List.map string_of_int p3)))

let rec is_sorted l =
  match l with
  | [] -> true
  | [h] -> true
  | h1::h2::t -> h1 <= h2 && (is_sorted (h2::t))

let state_is_valid (p1, p2, p3) =
  is_sorted p1 && is_sorted p2 && is_sorted p3


let do_move (p1, p2, p3) (from_p, to_p) =
  let (h, p1, p2, p3) =
    if from_p = 1 then
      match p1 with h::t -> (h, t, p2, p3) | _ -> raise InvalidMove
    else if from_p = 2 then
      match p2 with h::t -> (h, p1, t, p3) | _ -> raise InvalidMove
    else if from_p = 3 then
      match p3 with h::t -> (h, p1, p2, t) | _ -> raise InvalidMove
    else raise InvalidMove
  in
  if to_p = 1 then (h::p1, p2, p3)
  else if to_p = 2 then (p1, h::p2, p3)
  else if to_p = 3 then (p1, p2, h::p3)
  else raise InvalidMove


let rec print_moves start l =
  (print_hanoi start;
   if state_is_valid start then
     ()
   else
     raise InvalidMove);
  match l with
  | [] -> ()
  | move::t -> print_moves (do_move start move) t

(* General version of the Towers of Hanoi problem *)
(* Produce a list of moves to move n rings from from_peg to to_peg *)
(* Assumes the current state is valid. *)
let rec hanoi_general n from_peg to_peg other_peg : (int * int) list  =
  

let hanoi n =
  hanoi_general n 1 3 2

let print_from_start n l =
  print_moves (List.init n (fun i -> i), [], []) l

let main () =
  let arg =
    try Sys.argv.(1)
    with Invalid_argument _ -> (Printf.eprintf "Usage: hanoi <N>\n"; exit 1)
  in
  let n =
    try int_of_string arg
    with Failure _ -> (Printf.eprintf "Usage: hanoi <N>\n"; exit 1)
  in
  let moves = hanoi n in
  try print_from_start n moves
  with InvalidMove -> (Printf.eprintf "Error: invalid move produced!\n"; exit 1)

let _ = main ()
