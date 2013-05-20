(* ***************************************************************** *)
(* Structure union-find a la 'objet' utilisant les tables de hachage *)
(* ***************************************************************** *)

type ('a, 'b) unionFind = {union : 'a -> 'b -> unit; find : 'a -> 'b; (*iter : ('a -> 'b -> unit) -> unit*)};;

let create () =

  (* La valeur 257 est arbitraire dans l'ordre de grandeur du nombre de predicats. *)
  let table = Hashtbl.create 257 in
  let search x = try Hashtbl.find table x with Not_found -> Hashtbl.add table x x; x in 

  (* La recherche... *)
  let rec find i =
    if search i = i then i
    else begin
      let j = find (search i) in
        Hashtbl.replace table i j; j
    end in
  (* ...et l'union. *)
  let union i j =
    if compare (search i) (search j) < 0 then
      Hashtbl.replace table (search (find j)) (search i)
    else
      Hashtbl.replace table (search (find i)) (search j) in

  {union = union; find = find; (*iter = fun f -> Hashtbl.iter f table*)};;


(* ************** *)
(* Le SMT checker *)
(* ************** *)

exception Inconsistent;;

type terms = Fun of string * terms list | Cst of string
type predicat = Equal of terms * terms | Diff of terms * terms;;
type formule = Pred of predicat | Or of formule * formule | And of formule * formule | Not of formule | Imply of formule * formule;;

(* Verifie la coherence de l'arite des symboles de fonction. *)
let arity =
  let table = Hashtbl.create 257 in
  let search x n = try Hashtbl.find table x with Not_found -> Hashtbl.add table x n; n in 
    fun x n -> if search x n <> n then raise Inconsistent;;


(* Met a jour les structure union-find tout en verifiant la coherence. *)
let check pred eq df = 

  (* Dans le cas de l'egalite... *)
  let rec equal = function
    |(Cst a , Cst b) -> eq.union (Cst a) (Cst b)
    |(Fun (a, l1), Fun (b, l2)) -> arity a (List.length l1); arity b (List.length l2);
      eq.union (Fun (a, l1)) (Fun (b, l2)); iter_equal (l1, l2)
    |_ -> raise Inconsistent
  and iter_equal = function
    |([], []) -> ()
    |(a::l1, b::l2) -> equal (a, b) ; iter_equal (l1, l2)
    |_ -> raise Inconsistent in

  (* ...et de l'inegalite. *)
  let rec diff = function
    |(Cst a , Cst b) -> df.union (Cst a) (Cst b)
    |(Fun (a, l1), Fun (b, l2)) ->  arity a (List.length l1); arity b (List.length l2);
      df.union (Fun (a, l1)) (Fun (b, l2)); iter_diff (l1, l2)
    |(t1, t2) -> df.union t1 t2
  and iter_diff = function
    |([], []) -> ()
    |(a::l1, b::l2) -> diff (a, b) ; iter_diff (l1, l2)
    |_ -> () in

  (* Lors d'une union pour l'egalite on verifie la disjonction pour l'inegalite, et vice et versa. *)
  match pred with
    |Equal(t1, t2) -> if t1 <> t2 && eq.find (df.find t1) = eq.find (df.find t2)
      then raise Inconsistent else equal (t1, t2); eq.union t1 t2
    |Diff(t1, t2) -> if t1 = t2 || df.find (eq.find t1) = df.find (eq.find t2)
      then raise Inconsistent else diff (t1, t2); df.union t1 t2;;


(* Tests *)


let eq = create ()
and df = create ();;
check (Equal(Cst"a", Cst"b")) eq df;;
check (Diff(Cst"c", Cst"b")) eq df;;
check (Equal(Cst"a", Cst"c")) eq df;;

(*
let test = create ();;
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
