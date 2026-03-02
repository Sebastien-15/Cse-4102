let (x, y) = (42, 21) in x + y;;

match (42, 21) with
  (x, y) -> x + y
;;

let f (x, y) = x + 1;;


let rec sum l =
  match l with
  | [] -> 0
  | h::t -> h + (sum t)
;;

let rec inc_fst (l : (int * int) list) =
  match l with
  | [] -> []
  | (x, y)::t -> (x + 1, y)::(inc_fst t)
;;

let five = 5;;

let plusone = (fun x -> x + 1);;

let f = fun y -> plusone;;

let apply_twice (f: 'a -> 'a) x =
  f (f x)
;;

let compose (f : 'a -> 'b) (g: 'b -> 'c) x =
  g (f x)
;;

let plus_one_then_double x = compose ((+) 1) ( ( * ) 2) x;;

let plus_one_then_double = compose ((+) 1) ( ( * ) 2);;

let rec inc_all l =
  match l with
  | [] -> []
  | h::t -> (h + 1)::(inc_all t)

let rec square_all l =
  match l with
  | [] -> []
  | h::t -> (h * h)::(square_all t)

let rec make_excited_all l =
  match l with
  | [] -> []
  | h::t -> (h ^ "!")::(make_excited_all t)

let rec length_all l =
  match l with
  | [] -> []
  | h::t -> (String.length h)::(length_all l)
      

let rec map (f: 'a -> 'b) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | h::t -> (f h)::(map f t)

let inc_all = map ((+) 1)

let length_all = map String.length

let make_excited_all = map (fun s -> s ^ "!")

let inc_all = List.map ((+) 1)


let rec sum l =
  match l with
  | [] -> 0
  | h::t -> h + (sum t)

let rec product l =
  match l with
  | [] -> 1
  | h::t -> h * (product t)

let rec concat (ll : 'a list list) : 'a list =
  match ll with
  | [] -> []
  | h::t -> h @ (concat t)

(* Higher order functions *)
let rec fold (f: 'a -> 'b -> 'b) (l: 'a list) (u : 'b) : 'b =
  match l with
  | [] -> u
  | h::t -> f h (fold f t u)

let sum l = List.fold_right (+) l 0
let product l = List.fold_right ( * ) l 1
let concat l = List.fold_right (@) l []

let f l = List.fold_right (fun elem f_ap_rest -> elem::f_ap_rest) l []

let map f l = List.fold_right (fun elem mapped_rest -> (f elem)::mapped_rest) l []

let g l = List.fold_right (fun elem g_ap_rest -> g_ap_rest @ [elem]) l [] (* Reverse *)

let rec only_even (l: int list) : int list =
  match l with
  | [] -> []
  | h::t -> if h mod 2 = 0 then h::(only_even t) else only_even t

let rec only_nonempty (l : 'a list list) : 'a list list =
  match l with
  | [] -> []
  | h::t -> if h <> [] then h::(only_nonempty t) else only_nonempty t

let rec filter (f: 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | h::t -> if f h then h::(filter f t) else (filter f t)

let only_even =
  List.filter (fun h -> h mod 2 = 0)

let only_nonempty =
  List.filter (fun l -> match l with [] -> false | _ -> true)
