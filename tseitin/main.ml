open Formule
module Solution = Solution.Make

let compt = ref 1;;
(* compt donne une variable non-utilisée, après incrémentation. *)

module Assoc = Map.Make (struct
  type t = string
  let compare = compare
end);;

let table = ref Assoc.empty;;

let channel = try open_out "../Test/assoc.txt" with _ -> open_out "Test/assoc.txt";;

let var x = try Assoc.find x !table
  with Not_found -> (incr compt; 
    table := Assoc.add x !compt !table;
    Printf.fprintf channel "%s : %d\n" x !compt;
    flush channel;
    !compt);;

let tseitin form = 
  let rec aux x0 = function
    | Or (f1, f2) -> let x1, x2 = !compt + 1, !compt + 2 in compt := !compt + 2;
		     Solution.add_clause [-x0; x1; x2];
		     Solution.add_clause [x0; -x1];
		     Solution.add_clause [x0; -x2];
		     aux x1 f1; aux x2 f2 
    | And (f1, f2) -> let x1, x2 = !compt + 1, !compt + 2 in compt := !compt + 2;
		      Solution.add_clause [x0; -x1; -x2];
		      Solution.add_clause [-x0; x1];
		      Solution.add_clause [-x0; x2]; 
		      aux x1 f1; aux x2 f2
    | Not (f1) -> incr compt; let x1 = !compt in
		  Solution.add_clause [-x0; -x1];
		  Solution.add_clause [x0; x1];
		  aux x1 f1
    | Imply (f1, f2) -> aux x0 (Or (Not (f1), f2))
    | Var (x) -> let x1 = var x in
		 Solution.add_clause [-x0; x1]; 
		 Solution.add_clause [x0; -x1]
  in Solution.add_clause [!compt]; aux !compt form;;
(* Prend en entrée un arbre représentant une formule, et applique la transformation de tseitin
   pour remplir une formule sous forme normale conjonctive équivalente. *)


let print_solution (b,t) =
  if b then begin
    print_string "SATISFIABLE\n";
    Assoc.iter (fun var k -> print_string (var^" : "); if t.(k-1) > 0 then print_string "true\n" else print_string "false\n") (!table)
  end
  else print_string "UNSATISFIABLE\n";;

let main () =
  let s = ref "../Test/form0.cnf" and aff = ref true in
  Arg.parse [("-naff", Arg.Unit (fun () -> aff := false), "Ne pas afficher le résultat en console")] (fun x -> s := x) "";
  let s1 = ref "../Test/tseitin.cnf" in
  let file = try open_out !s1 with _ -> s1 := "Test/tseitin.cbf"; open_out !s1 in
  let channel = open_in !s in
  let lexbuf = Lexing.from_channel channel in
  let form = Parser.form Lexer.lexer lexbuf in
  tseitin form;
  Solution.fix !compt;
  Solution.write file;
  let _ = try Sys.command ("./../dpll -naff "^(!s1)) with _ ->  Sys.command ("./dpll -naff "^(!s1)) in
  if !aff then print_solution (Solution.read (try Scanf.Scanning.open_in "../Test/result.txt" with _ -> Scanf.Scanning.open_in "Test/result.txt"));;

main ()
