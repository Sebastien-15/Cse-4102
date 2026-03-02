type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec map_tree (f : 'a -> 'b) (t : 'a tree) : 'b tree =
  match t with
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map_tree f l, map_tree f r)

let t = Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Leaf))

let t1 = map_tree ((+) 1) t

let rec fold_tree (f : 'a -> 'b -> 'b -> 'b) (u : 'b) (t : 'a tree) : 'b =
  match t with
  | Leaf -> u
  | Node (v, l, r) -> f v (fold_tree f u l) (fold_tree f u r)

let size = fold_tree (fun _ size_l size_r -> size_l + size_r + 1) 0

let size_t = size t

let preorder = fold_tree
                 (fun v preorder_l preorder_r -> v :: (preorder_l @ preorder_r))
                 []

let mirror = fold_tree
               (fun v mirror_l mirror_r -> Node (v, mirror_r, mirror_l))
               Leaf

type 'a twothreetree =
  | Leaf23
  | Node2 of 'a * 'a twothreetree * 'a twothreetree
  | Node3 of 'a * 'a twothreetree * 'a twothreetree * 'a twothreetree

let rec fold_23tree
          (f : 'a -> 'b -> 'b -> 'b)
          (g : 'a -> 'b -> 'b -> 'b -> 'b)
          (u : 'b)
          (t : 'a twothreetree) : 'b =
  match t with
  | Leaf23 -> u
  | Node2 (v, l, r) -> f v (fold_23tree f g u l) (fold_23tree f g u r)
  | Node3 (v, l, m, r) ->
     g v (fold_23tree f g u l) (fold_23tree f g u m) (fold_23tree f g u r)

let size23 = fold_23tree
               (fun _ size_l size_r -> size_l + size_r + 1)
               (fun _ size_l size_m size_r -> size_l + size_m + size_r + 1)
               0

let rec map f l =
  match l with
  | [] -> []
  | x::t -> (f x)::(map f t)

let l = List.init 100_000_000 (fun i -> i)
let l2 = map (fun x -> x * 2) (List.init 100_000_000 (fun i -> i))

let rev l =
  let rec rev_inner a l =
    match l with
    | [] -> a
    | x::t -> rev_inner (x::a) t
  in
  rev_inner [] l

let map_tr f l =
  let rec map_inner a l =
    match l with
    | [] -> 0::(rev a)
    | x::t -> map_inner ((f x)::a) t
  in
  map_inner [] l

let l2 = map_tr (fun x -> x * 2) (List.init 100_000_000 (fun i -> i))

let sum_tr (l : int list) : int =
  let rec sum_inner a l =
    match l with
    | [] -> a
    | x::t -> sum_inner (x + a) t
  in
  sum_inner 0 l

let fold_tr (f: 'b -> 'a -> 'b) (u: 'b) (l: 'a list) =
  let rec fold_inner f a l =
    match l with
    | [] -> a
    | x::r -> fold_inner f (f a x) r
  in
  fold_inner f u l

let sum = List.fold_left (+) 0

let mystery = List.fold_left (fun mystery_rest x -> x::mystery_rest) []
