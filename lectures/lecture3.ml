(1, 2) : int * int (* NOT (int, int) *);;
(1, 1 + 1);;
(1, 1 + 1, 1 + 1 + 1);;
1, 2;;
(1, "Hello", 'a', true);;
((1, 2), (true, ("Hi", "World")));;

fst (1, 2);;
(* Ill-typed: fst (1, 2, 3);; *)
snd (1, 2);;

let (x, y) = if 3 > 4 then (1, 2) else (3, 4);;

let fst3 (triple : 'fst * 'snd * 'td) : 'fst =
  let (x, y, z) = triple in x

let fst3 (x, _, _) = x

let id x = x

let swap (x, y) = (y, x)
;;

0 :: [];;
[0];;
[1; 2; 3; 4; 5];;
1 :: 2 :: 3 :: (4 :: (5 :: []));;
['a'; 'b'; 'c'];;
[1; 'a'];;
[(1, 2); (3, 4)];;
[[]; [1; 2]; [1; 2; 3]];;

[1; 2] @ [3; 4];;
List.append [1; 2] [3; 4];;

let hd (l: int list) : int =
  match l with
  | [] -> 0
  | h::_ -> h

let inc_first (l: int list) : int list =
  match l with
  | [] -> []
  | h::t -> (h + 1)::t

let l = [1; 2; 3];;

inc_first l;;

l;;

let rec inc_all (l: int list) : int list =
  match l with
  | [] -> []
  | h::t -> (h + 1)::(inc_all t)
;;

let rec sum (l: int list) : int =
  match l with
  | [] -> 0
  | h::t -> (sum t) + h
;;

let rec is_sorted (l : int list) : bool =
  match l with
  | [] -> true
  | [h1] -> true
  | h1::h2::t -> (h1 <= h2) && is_sorted (h2::t)

let rec append (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with
  | [] -> l2
  | h::t -> h::(append t l2)

let rec rev (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> append (rev t) [h]
;;
