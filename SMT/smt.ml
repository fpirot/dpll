(* ***************************************************************** *)
(* Structure union-find a la 'objet' utilisant les tables de hachage *)
(* ***************************************************************** *)

type ('a, 'b) unionFind = {union : 'a -> 'b -> unit; find : 'a -> 'b; (*update : unit -> unit; iter : ('a -> 'b -> unit) -> unit*)};;

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

  (*let update () =
    Hashtbl.iter (fun x y -> let _ = find y in ()) table in*)

  {union = union; find = find; (*update = update ; iter = fun f -> Hashtbl.iter f table*)};;



(* ************** *)
(* Le SMT checker *)
(* ************** *)

exception Inconsistent;;

type terms = Fun of string * terms list | Var of string;;
type predicat = Equal of terms * terms | Diff of terms * terms;;


let check pred eq df = 

  let rec equal = function
    |(Var a , Var b) -> eq.union (Var a) (Var b)
    |(Fun (a, l1), Fun (b, l2)) -> eq.union (Fun (a, l1)) (Fun (b, l2)); iter_equal (l1, l2)
    |_ -> raise Inconsistent
  and iter_equal = function
    |([], []) -> ()
    |(a::l1, b::l2) -> equal (a, b) ; iter_equal (l1, l2)
    |_ -> raise Inconsistent in

  let rec diff = function
    |(Var a , Var b) -> df.union (Var a) (Var b)
    |(Fun (a, l1), Fun (b, l2)) -> df.union (Fun (a, l1)) (Fun (b, l2)); iter_diff (l1, l2)
    |(t1, t2) -> df.union t1 t2
  and iter_diff = function
    |([], []) -> ()
    |(a::l1, b::l2) -> diff (a, b) ; iter_diff (l1, l2)
    |_ -> () in

  match pred with
    |Equal(t1, t2) -> if t1 <> t2 && eq.find (df.find t1) = eq.find (df.find t2)
      then raise Inconsistent else equal (t1, t2); eq.union t1 t2
    |Diff(t1, t2) -> if t1 = t2 || df.find (eq.find t1) = df.find (eq.find t2)
      then raise Inconsistent else diff (t1, t2); df.union t1 t2;;

let eq = create 10
and df = create 10;;
check (Equal(Fun("f", [Var"a"]), Fun("g", [Var"b"]))) eq df;;
check (Diff(Fun("g", [Var"c"]), Fun("h", [Var"b"]))) eq df;;
check (Equal(Fun("h", [Var"a"]), Fun("f", [Var"c"]))) eq df;;

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
