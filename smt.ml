(* ***************************************************************** *)
(* Structure union-find a la 'objet' utilisant les tables de hachage *)
(* ***************************************************************** *)

type ('a, 'b) unionFind = {union : 'a -> 'b -> unit; find : int -> int(*; iter : ('a -> 'b -> unit) -> unit*)};;

let create n =

  let table = Hashtbl.create n in
  let search x = try Hashtbl.find table x with Not_found -> Hashtbl.add table x x; x in 

  let rec find i =
    if search i = i then i
    else begin
      let j = find (search i) in
        Hashtbl.replace table i j; j
    end in

  let union i j =
    if compare (search i) (search j) < 0 then
      Hashtbl.replace table (search (find j)) (search i)
    else
      Hashtbl.replace table (search (find i)) (search j) in

  {union = union; find = find(*; iter = fun f -> Hashtbl.iter f table*)};;



(* ************** *)
(* Le SMT checker *)
(* ************** *)

exception Inconsistent;;

type terms = Fun of terms list | Var of string
type predicat = Equal of terms * terms | Diff of terms * terms

let rec equal = function
  |(Var a , Var b) -> a = b
  |(Fun l1, Fun l2) -> iter_equal (l1, l2)
  |_ -> false
and iter_equal = function
  |([], []) -> true
  |(a::l1, b::l2) -> equal (a, b) && iter_equal (l1, l2)
  |_ -> false;;

let rec diff = function
  |(Var a , Var b) -> not (a = b)
  |(Fun l1, Fun l2) -> iter_diff (l1, l2)
  |_ -> true
and iter_diff = function
  |([], []) -> true
  |(a::l1, b::l2) -> diff (a, b) && iter_diff (l1, l2)
  |_ -> true;;

let check pred = match pred with
  |Equal(t1, t2) -> equal (t1, t2)
  |Diff(t1, t2) -> diff (t1, t2);;

let p = Equal(Fun([Var"a"]), Fun([]));;
check p;;

(*
type unionFind = {union : int -> int -> unit; find : int -> int; print : unit -> unit};;

let create n =

  let arrayCls = Array.init n (fun i -> i) in
  let print () = Array.iteri (fun i x -> if i = 0 then print_int x
      else begin print_string "; "; print_int x end) arrayCls in

  let rec find i =
    if arrayCls.(i) = i then i
    else begin
      let j = find arrayCls.(i) in
        arrayCls.(i) <- j; j
    end in

  let union i j =
    if arrayCls.(i) < arrayCls.(j) then
      arrayCls.(find j) <- arrayCls.(i)
    else
      arrayCls.(find i) <- arrayCls.(j) in

  {union = union; find = find; print = print};;
*)

(*
let test = create 10;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.union 5 4;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.union 6 7;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.union 5 7;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.union 2 4;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.union 1 4;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.union 1 0;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
test.find 7;;
test.iter (fun a b -> print_int a; print_string "->"; print_int b; print_string " ");;
*)
