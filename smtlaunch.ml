open Smt;;

let main () =
let s = ref "Test/formule.txt" in
Arg.parse [] (fun x -> s := x) "";
let file = open_in !s in
Convert.main file;
let _ = Sys.command "./tseitin/tseitin -naff Test/smt.cnf" in
Make.print_solution ();;

main();;
