let r = ref 42;;
let _ = r := 43;;
let _ = r := (!r) + 1;;

let x =
    let my_ref = ref 0 in
    (!my_ref) + (my_ref := 1; 0)
;;

let r = ref 0 in
  let a =
     let r = ref 0 in
     (r := 1;
     !r)
  in
  let b = !r in
  a + b
;;

let a = ref 0 in
let b = ref (!a) in
a := 1;
!b
;;

let l = ref [1; 2; 3; 4; 5]
let _ = l := [1; 2; 3; 4; 5; 6; 7; 8]

type 'a dll_node =
  {value : 'a;
   mutable next : 'a dll;
   mutable prev : 'a dll}
and 'a dll =
  | Empty
  | Head of 'a dll_node

let empty_dll = Empty

let cons_dll (x: 'a) (l: 'a dll) : 'a dll =
    match l with
    | Empty -> Head { value = x; next = Empty; prev = Empty}
    | Head node ->
        let new_node =
            { value = x;
              next = l;
              prev = Empty;
            }
        in
        node.prev <- Head new_node;
        Head new_node
;;

let rec print_dll (l: int dll) =
    match l with
    | Empty -> ()
    | Head node ->
        Printf.printf "%d " node.value;
        print_dll node.next
;;

let evil =
    let l = {value = 1; next = Empty; prev = Empty} in
    l.next <- Head l;
    Head l
;;

(* let _ = print_dll evil;; *)

let a = Array.init 1000 (fun i -> i)
let _ = a.(500) <- 42
let _ = Printf.printf "%d\n" (a.(500));;

let rec f x =
    if x <= 0 then 0
    else g x
and g x = f (x - 1)



let rec f x = 5