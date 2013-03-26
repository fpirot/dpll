module Load =
struct
	
	let rec clause channel = function
		|[] -> Printf.fprintf channel "0\n"
		|a::l -> Printf.fprintf channel "%d " a; clause channel l
	
	let file n v c s lst =
		let channel = open_out (s^(string_of_int n)) in
		Printf.fprintf channel "p cnf %d %d\n" v c;
		List.iter (fun l -> clause channel l) lst;
		close_out channel
		
end;;


module Test =
struct
	
	let (mode, nbr, var, cls, size, path) =
      let s1 = ref 4
      and s2 = ref 6
      and v1 = ref 20
      and v2 = ref 30
      and c1 = ref 100
      and c2 = ref 200
      and n = ref 100
      and m = ref ""
      and p = ref "Test/" in
        Arg.parse [("-setc", Arg.Tuple [Arg.Set_int c1; Arg.Set_int c2], "Number of clauses");
        	("-sets", Arg.Tuple [Arg.Set_int s1; Arg.Set_int s2], "Size of clauses");
        	("-setv", Arg.Tuple [Arg.Set_int v1; Arg.Set_int v2], "Number of variables");
        	("-default", Arg.Tuple [Arg.Set_int v1; Arg.Set_int c1; Arg.Set_int s1], "Change default value of clause, variable and size");
        	("-clause", Arg.Unit(fun () -> m := "clause"), "Select the clause creation mode");
        	("-size", Arg.Unit(fun () -> m := "size"), "Select the size creation mode");
        	("-variable", Arg.Unit(fun () -> m := "variable"), "Select the variable creation mode");
        	("-test", Arg.Set_int n, "Number of tests")]
            (fun str -> ()) "";
        (!m, !n, (!v1, !v2), (!c1, !c2), (!s1, !s2), !p)

	let random_literal v = 
		Random.self_init();
		let n = (Random.int v) + 1
		and b = Random.bool() in
			if b then -n else n

	let rec random_clause v = function
		|0 -> []
		|s -> (random_literal v) :: (random_clause v (s - 1))

	let rec random_sat s v = function
		|0 -> []
		|c -> (random_clause v s)::(random_sat s v (c - 1))
	
	let random () = match mode with
		|"clause" -> let c = max 1 (((snd cls) - (fst cls))/nbr) in
			for k = 0 to nbr do
				Load.file k (fst var) (c * k + (fst cls)) path (random_sat (fst size) (fst var) (c * k + (fst cls)))
			done
		|"variable" -> let v = max 1 (((snd var) - (fst var))/nbr) in
			for k = 0 to nbr do
				Load.file k (v * k + (fst var)) (fst cls) path (random_sat (fst size) (v * k + (fst var)) (fst cls))
			done
		|"size" -> let s = max 1 (((snd size) - (fst size))/nbr) in
			for k = 0 to nbr do
				Load.file k (fst var) (fst cls) path (random_sat (s * k + (fst size)) (fst var) (fst cls))
			done
		|_ -> print_string "Please, select a mode of creation with [-clause | -variable | -size]\n"
	
end;;

Test.random ();
print_int (Sys.command "./run-tests.sh");
print_newline();
print_int (Sys.command "gnuplot plot.p");
print_newline();;
