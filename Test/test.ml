module Load =
struct
  
  let rec clause channel = function
    |[] -> Printf.fprintf channel "0\n"
    |a::l -> Printf.fprintf channel "%d " a; clause channel l
      
  let file (n, m) v c s lst =
    let channel = open_out (s^(string_of_int n)^"/"^(string_of_int m)) in
    Printf.fprintf channel "p cnf %d %d\n" v c;
    List.iter (fun l -> clause channel l) lst;
    close_out channel
      
end;;


module Test =
struct
  
  let nbr = 10
  let (mode, prc, var, cls, size, path) =
    let s1 = ref 4
    and s2 = ref 6
    and v1 = ref 20
    and v2 = ref 30
    and c1 = ref 100
    and c2 = ref 1000
    and n = ref 10
    and m = ref ""
    and p = ref "Tmp/" in
    Arg.parse [("-setc", Arg.Tuple [Arg.Set_int c1; Arg.Set_int c2], "Number of clauses");
               ("-sets", Arg.Tuple [Arg.Set_int s1; Arg.Set_int s2], "Size of clauses");
               ("-setv", Arg.Tuple [Arg.Set_int v1; Arg.Set_int v2], "Number of variables");
               ("-default", Arg.Tuple [Arg.Set_int v1; Arg.Set_int c1; Arg.Set_int s1], "Change default value of clause, variable and size");
               ("-clause", Arg.Unit(fun () -> m := "clause"), "Select the clause creation mode");
               ("-size", Arg.Unit(fun () -> m := "size"), "Select the size creation mode");
               ("-variable", Arg.Unit(fun () -> m := "variable"), "Select the variable creation mode");
               ("-precision", Arg.Set_int n, "Number of tests")]
      (fun str ->()) "";
    (!m, !n, (!v1, !v2), (!c1, !c2), (!s1, !s2), !p)

  let start () =
    let _ = try (if Sys.is_directory "Tmp/" then Sys.command "rm -R Tmp/*" else 0) with _ -> Sys.command "mkdir Tmp" in
    for k = 0 to nbr do
      let _ = Sys.command ("mkdir Tmp/"^(string_of_int k)) in ()
    done

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
      
  let random () =
    start ();
    match mode with
      |"clause" ->
	let c = (snd cls) - (fst cls)
	and v = ((fst cls) + (snd cls))/8 in
	for k = 0 to nbr do
	  for l = 0 to prc - 1 do
	    Load.file (k, l) v ((c * k + (fst cls) * 100)/100) path (random_sat 3 v ((c * k + (fst cls) * 100)/100))
	  done done
      |"variable" ->
	let v = (snd var) - (fst var)
	and c = ((fst var) + (snd var))*8 in
	for k = 0 to nbr do
	  for l = 0 to prc - 1 do
	    Load.file (k, l) ((v * k + (fst var) * 100)/100) c path (random_sat 3 ((v * k + (fst var) * 100)/100) c)
	  done done
      |"size" -> let s = (snd size) - (fst size) in
		 for k = 0 to nbr do
		   for l = 0 to prc - 1 do
		     Load.file (k, l) (fst var) (fst cls) path (random_sat ((s * k + (fst size) * 100)/100) (fst var) (fst cls))
		   done done
      |_ -> failwith "Please, select a mode of creation with [-clause | -variable | -size]\n"
	
end;;

try
  (begin
    print_newline ();
    Test.random ();
    print_int (Sys.command "./run.sh");
    print_newline();
    print_int (Sys.command "gnuplot plot.p");
    print_newline()
   end)
with Failure s -> print_string s;;
