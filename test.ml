module Solution = Solution.Make;;

Random.self_init();;

let random_literal n =
  let p = (Random.int n) + 1 in
  let b = Random.int 2 in
  if b = 0 then -p else p;;

let rec random_clause n = function
  |0 -> []
  |k -> (random_literal n) :: (random_clause n (k - 1));;
(* Génère une clause de taille t, aléatoire. *)

let random_sat n p k =
Solution.fix n;
for i = 1 to p do Solution.add_clause (random_clause n k) done;;
(* n est le nombre de variables, p est le nombre de clauses. *)

let main() = 
  let n, p, k, s = ref 20, ref 100, ref 3, ref "" in
  Arg.parse [("-setv", Arg.Tuple [Arg.Set_int n; Arg.Unit (fun () -> p := truncate (4.13 *. float !n))], "Number of variables");
	     ("-setc", Arg.Set_int p, "Number of clauses");
	     ("-sets", Arg.Set_int k, "Size of the clauses");
	     ("-wlit", Arg.Unit (fun () -> s := (!s)^" -wlit"), "Watched Literals");
	     ("-rand", Arg.Unit (fun () -> s := (!s)^" -rand"), "Random choice");
	     ("-moms", Arg.Unit (fun () -> s := (!s)^" -moms"), "Most Occurences in clauses of Minimum Size");
	     ("-dlis", Arg.Unit (fun () -> s := (!s)^" -dlis"), "Dynamic Largest Individual Sum");
	     ("-graph", Arg.Unit (fun () -> s := (!s)^" -graph"), "Displays the graph options")] (fun x -> ()) "";
  let file = open_out "Test/ex0.cnf" in
  random_sat !n !p !k;
  Solution.write file;
  let _ = Sys.command ("./dpll"^(!s)) in ();;

main();;
