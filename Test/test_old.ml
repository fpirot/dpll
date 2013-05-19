(*********************************************************)
(*        Ecriture d'une instance dans un fichier        *)
(*********************************************************)

type sat = {k : int; clauses : int list list};;

let rec string_of_clause = function
  | [] -> "0\n"
  | x :: r -> (string_of_int x)^" "^(string_of_clause r);;

let output_sat file sat = 
  let n = sat.k in
  let p = List.length sat.clauses in
  output_string file ("p cnf "^(string_of_int n)^" "^(string_of_int p)^"\n");
  let c = ref sat.clauses in
  for i = 1 to p do
    let s = string_of_clause (List.hd !c) in
    output_string file s; c := List.tl !c
  done;;


(************************************************************)
(*         Génération d'une instance 3-SAT aléatoire        *)
(************************************************************)

let random_literal n = 
  Random.self_init();
  let p = (Random.int n) + 1 in
  let b = Random.int 2 in
  if b = 0 then -p else p;;

let rec random_clause n = function
  |0 -> []
  |t -> (random_literal n) :: (random_clause n (t - 1))
(* Génère une clause de taille t, aléatoire. *)

let random_sat n p t =
  let rec aux = function
    | 0 -> []
    | p -> (random_clause n t) :: (aux (p-1))
  in {k = n; clauses = aux p};;
(* n est le nombre de variables, p est le nombre de clauses. *)

let make_test n =
  let rec make_clause = function
    | 1 -> [1]
    | 2 -> [2]
    | n -> n :: (make_clause (n-2))
  in let rec aux = function
    | 0 -> []
    | n -> (n :: (make_clause (n - (n mod 2)))) :: (-n :: (make_clause (n + (n mod 2) -1))) :: (aux (n-1))
     in {k = n; clauses = aux n};;

let main () =
  let n = try int_of_string (Sys.argv).(1) with _ -> 20 in
  let k = try n * int_of_string (Sys.argv).(2) with _ -> int_of_float (4.3 *. float n) in
  let t = try int_of_string (Sys.argv).(3) with _ -> 3 in
  let file = open_out "ex0.cnf" in
  let s = random_sat n k t in
  output_sat file s;;

main();;
