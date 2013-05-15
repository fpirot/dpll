open Formule
module Solution = Solution.Make

let compt = ref 0;;
(* compt donne une variable non-utilisée, après incrémentation. *)

module Assoc = Map.Make (struct
  type t = string
  let compare = compare
end);;

let table = ref Assoc.empty;;

let var x = try Assoc.find x !table
  with Not_found -> (incr compt; 
    table := Assoc.add x !compt !table;
    !compt);;

let tseitin form = 
  let rec aux = function
    | Or (f1, f2) -> let x0, x1, x2 = !compt + 1, !compt + 2, !compt + 3 in compt := !compt + 3;
		     Solution.add_clause [-x0; x1; x2];
		     Solution.add_clause [x0; -x1];
		     Solution.add_clause [x0; -x2];
		     aux f1; aux f2 
    | And (f1, f2) -> let x0, x1, x2 = !compt + 1, !compt + 2, !compt + 3 in compt := !compt + 3;
		      Solution.add_clause [x0; -x1; -x2];
		      Solution.add_clause [-x0; x1];
		      Solution.add_clause [-x0; x2]; 
		      aux f1; aux f2
    | Not (f1) -> let x0, x1 = !compt + 1, !compt + 2 in compt := !compt + 2;
		  Solution.add_clause [-x0; -x1];
		  Solution.add_clause [x0; x1];
		  aux f1
    | Imply (f1, f2) -> aux (Or (Not (f1), f2))
    | Var (x) -> let x0, x1 = var x, !compt + 1 in incr compt;
		 Solution.add_clause [-x0; x1]; 
		 Solution.add_clause [x0; -x1]
  in  Solution.add_clause [1]; aux form;;
(* Prend en entrée un arbre représentant une formule, et applique la transformation de tseitin
   pour remplir une formule sous forme normale conjonctive équivalente. *)


let print_solution (b,t) =
  if b then begin
    print_string "SATISFIABLE\n";
    Assoc.iter (fun var k -> print_string (var^" : "); if t.(k) > 0 then print_string "true\n" else print_string "false\n") (!table)
  end
  else print_string "UNSATISFIABLE\n";;

let main () =
let file = open_out "../Test/tseitin.cnf" in
let channel = open_in (try Sys.argv.(1) with _ -> "../Test/form0.cnf") in
let lexbuf = Lexing.from_channel channel in
let form = Parser.form Lexer.lexer lexbuf in
tseitin form;
Solution.fix !compt;
Solution.write file;
let _ = Sys.command "./../dpll ../Test/tseitin.cnf" in
print_solution (Solution.read (Scanf.Scanning.open_in "../Test/result.txt"));;

main ()
