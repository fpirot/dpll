open Solution;;
open Type;;
open Convert;;

(* ***************************************************************** *)
(* Structure union-find a la 'objet' utilisant les tables de hachage *)
(* ***************************************************************** *)
type ('a, 'b) unionFind = {union : 'a -> 'b -> unit; find : 'a -> 'b};;
type ('a, 'b) set = {add :'a -> 'b -> unit; exists : 'a -> 'a -> bool};;

module Uf =
struct

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


  ({union = union; find = find;},
  {add = (fun x y -> add x y; add y x); exists = (fun x y -> exists x y || exists y x)})

end;;

(* ************** *)
(* Le SMT checker *)
(* ************** *)

module Smt = struct
exception Inconsistent of predicat list;;

(* Verifie la coherence de l'arite des symboles de fonction. *)
let arity =
  let table = Hashtbl.create 257 in
  let search x n = try Hashtbl.find table x with Not_found -> Hashtbl.add table x n; n in 
    fun x n -> if search x n <> n then failwith "Signature mismatch"

(* Nouvelle 'clause' obtenue en cas d'incoherence sur un egalite *)
let newEqual t1 t2 r1 r2 = [(Equal (r1, r2)); (Diff(r1, t1)); (Diff (r2, t2))]
(* Idem en cas d'incoherence sur une inegalite  *)
let newDiff t1 t2 r1 r2 = [(Equal (t1, t2)); (Diff(r1, t1)); (Diff (r2, t2))]

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
        then raise (Inconsistent (newDiff t1 t2 r1 r2)) else diff (t1, t2); df.add t1 t2
end;;


module Make = struct
  let validity file =
    let (b, t) = Solution.read file in
    let assoc =
      let table = Hashtbl.create 257
    (* table qui à une variable de tseitin associe la variable signée dans dpll. *)
      and channel = try Scanf.Scanning.open_in "Test/assoc.txt" with _ -> Scanf.Scanning.open_in "../Test/assoc.txt" in
      let rec read () =
	try Scanf.bscanf channel "x%d : %d\n" (fun x n -> Hashtbl.add table x t.(n-1); read ())
	with _  -> () in
      read();
      table in
    let (eq, df) = Uf.create () in
    try (Hashtbl.iter (fun x n -> match (Convert.table.read x, n > 0) with
      |Equal(a, b), true -> Smt.check (Equal(a, b)) eq df
      |Equal(a, b), false -> Smt.check (Diff(a, b)) eq df  
      |Diff(a, b), true -> Smt.check (Diff(a, b)) eq df   
      |Diff(a, b), false -> Smt.check (Equal(a, b)) eq df) assoc; [])
    with Smt.Inconsistent(lst) -> List.map (fun x -> Hashtbl.find assoc (Convert.table.write x)) lst;;

  let print_solution file =
    let (b, t) = Solution.read file in
    let assoc =
      let table = Hashtbl.create 257
    (* table qui à une variable de tseitin associe la variable signée dans dpll. *)
      and channel = try Scanf.Scanning.open_in "Test/assoc.txt" with _ -> Scanf.Scanning.open_in "../Test/assoc.txt" in
      let rec read () =
	try Scanf.bscanf channel "x%d : %d\n" (fun x n -> Hashtbl.add table x t.(n-1); read ())
	with _  -> () in
      read();
      table in
    if b then begin print_string "s SATISFIABLE\n";
      Hashtbl.iter (fun x n -> Convert.print_pred (Convert.table.read x) (n > 0)) assoc
    end else print_string "s UNSATISFIABLE\n";;
end;;
