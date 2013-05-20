open Solution;;
open Type;;
open Main;;

(* ***************************************************************** *)
(* Structure union-find a la 'objet' utilisant les tables de hachage *)
(* ***************************************************************** *)

type ('a, 'b) unionFind = {union : 'a -> 'b -> unit; find : 'a -> 'b; (*iter : ('a -> 'b -> unit) -> unit*)};;
type ('a, 'b) set = {add :'a -> 'b -> unit; exists : 'a -> 'a -> bool};;

let create () =

  (* La valeur 257 est arbitraire dans l'ordre de grandeur du nombre de predicats. *)
  let ufTable = Hashtbl.create 257 in
  let ufSearch x = try Hashtbl.find ufTable x with Not_found -> Hashtbl.add ufTable x x; x in 

  let setTable = Hashtbl.create 257 in  
  let setSearch x = try Hashtbl.find setTable x with Not_found -> Hashtbl.add setTable x []; [] in

  let exists x y = List.exists (fun z -> z = y) (setSearch x) in
  let add x y = let l = setSearch x in
      if List.exists (fun z -> z = y) l then () else Hashtbl.add setTable x (y::l) in
  let update x y =  List.iter (fun z -> add z y; add y z) (setSearch x) in
  
  (* La recherche... *)
  let rec find i =
    if ufSearch i = i then i
    else begin
      let j = find (ufSearch i) in
        Hashtbl.replace ufTable i j;
        update i j; j
    end in
  (* ...et l'union. *)
  let union i j =
    if compare (ufSearch i) (ufSearch j) < 0 then
      let j' = ufSearch (find j)
      and i' = ufSearch i in
        Hashtbl.replace ufTable j' i';
        update j' i';
    else
      let i' = ufSearch (find i)
      and j' = ufSearch j in
        Hashtbl.replace ufTable i' j';
        update i' j' in


  ({union = union; find = find; (*iter = fun f -> Hashtbl.iter f table*)},
  {add = (fun x y -> add x y; add y x); exists = (fun x y -> exists x y || exists y x)});;

(* ************** *)
(* Le SMT checker *)
(* ************** *)

exception Inconsistent of formule list;;

(* Verifie la coherence de l'arite des symboles de fonction. *)
let arity =
  let table = Hashtbl.create 257 in
  let search x n = try Hashtbl.find table x with Not_found -> Hashtbl.add table x n; n in 
    fun x n -> if search x n <> n then failwith "Signature mismatch";;

(* Nouvelle 'clause' obtenue en cas d'incoherence sur un egalite *)
let newEqual t1 t2 r1 r2 = [Pred(Equal (r1, r2)); Not(Pred(Equal(r1, t1)); Not(Pred(Equal (r2, t2)))];;
(* Idem en cas d'incoherence sur une inegalite  *)
let newDiff t1 t2 r1 r2 = [Pred(Equal (t1, t2)); Not(Pred(Equal(r1, t1))); Not(Pred(Equal (r2, t2)))];;

(* Met a jour les structure union-find tout en verifiant la coherence. *)
let check pred eq df = 

  (* Dans le cas de l'egalite... *)
  let rec equal = function
    |(Cst a , Cst b) -> eq.union (Cst a) (Cst b)
    |(Fun (a, l1), Fun (b, l2)) -> arity a (List.length l1); arity b (List.length l2);
      eq.union (Fun (a, l1)) (Fun (b, l2)); iter_equal (l1, l2)
    |_ -> failwith "Signature mismatch"
  and iter_equal = function
    |([], []) -> ()
    |(a::l1, b::l2) -> equal (a, b) ; iter_equal (l1, l2)
    |_ -> failwith "Signature mismatch" in

  (* ...et de l'inegalite. *)
  let rec diff = function
    |(Cst a , Cst b) -> df.add (eq.find (Cst a)) (eq.find (Cst b))
    |(Fun (a, l1), Fun (b, l2)) ->  arity a (List.length l1); arity b (List.length l2);
      df.add (eq.find (Fun (a, l1))) (eq.find (Fun (b, l2))); iter_diff (l1, l2)
    |(t1, t2) -> df.add (eq.find t1) (eq.find t2)
  and iter_diff = function
    |([], []) -> ()
    |(a::l1, b::l2) -> diff (a, b) ; iter_diff (l1, l2)
    |_ -> () in

  (* Lors d'une union pour l'egalite on verifie la disjonction pour l'inegalite, et vice et versa. *)
  match pred with
    |Equal(t1, t2) -> let r1 = eq.find t1 and r2 = eq.find t2 in
      if t1 <> t2 && df.exists r1 r2
        then raise (Inconsistent (newEqual t1 t2 r1 r2)) else equal (t1, t2); eq.union t1 t2
    |Diff(t1, t2) -> let r1 = eq.find t1 and r2 = eq.find t2 in
      if t1 = t2 || r1 = r2
        then raise (Inconsistent (newDiff t1 t2 r1 r2)) else diff (t1, t2); df.add t1 t2;;


let (_, t) = Solution.read (Scanf.Scanning.open_in "../Test/result.txt");;
let (valu, assoc) =
  let table = Hashtbl.create 257
  and autre = Hashtbl.create 257
  and channel = Scanf.Scanning.open_in "../Test/assoc.txt" in
  let rec read =
    try Scanf.Scanning.bscanf channel "x%d : %d "
      (fun x n -> Hashtbl.add table x (t.(abs n - 1) > 0); Hashtbl.add autre x n; read channel)
    with End_of_file -> () in
  (table, autre);;

let validity () =

  let (eq, df) = create () in
  try Hashtbl.iter (fun x b-> match Main.table.find x, b with
      |Equal(a, b), true -> check (Equal(a, b)) eq df
      |Equal(a, b), false -> check (Diff(a, b)) eq df  
      |Diff(a, b), true -> check (Diff(a, b)) eq df   
      |Diff(a, b), false -> check (Equal(a, b)) eq df) valu with
  Inconsistent(lst) -> List.map (fun x -> Hashtbl.find assoc (Main.table.add x)) lst;;
  


(* Tests *)

(*
let (eq, df) = create ();;
check (Equal(Cst"a", Cst"b")) eq df;;
check (Diff(Cst"c", Cst"b")) eq df;;
check (Equal(Cst"c", Cst"d")) eq df;;
check (Diff(Cst"a", Cst"d")) eq df;;
check (Equal(Cst"a", Cst"e")) eq df;;
check (Equal(Cst"c", Cst"e")) eq df;;
*)
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

(*
let uf () =

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



let set () =

  let table = Hashtbl.create 257 in  
  let search x = try Hashtbl.find table x with Not_found -> Hashtbl.add table x []; [] in
  
    let exists x y = List.exists (fun z -> z = y) (search x)
    and add x y = let l = search x in
      if List.exists (fun z -> z = y) l then () else Hashtbl.add table x (y::l) in
  
{add = (fun x y -> add x y; add y x); exists = (fun x y -> exists x y || exists y x)};;
*)
