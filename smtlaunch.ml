let main () =
let s = ref "Test/formule.txt" in
Arg.parse [] (fun x -> s := x) "";
let file = open_in !s in
Convert.main file;
let _ = Sys.command "./tseitin/tseitin -naff Test/smt.cnf" in
let channel = Scanf.Scanning.open_in "Test/result.txt" in
Smt.Make.print_solution channel;;

main();;
