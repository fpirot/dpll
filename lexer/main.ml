open Formule

type clause = int list;;
type sat = clause list;;
(* On représente la forme nomale conjonctive par une liste de clauses *)

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

let rec tseitin = function
  | Or (f1, f2) -> let x0, x1, x2 = !compt + 1, !compt + 2, !compt + 3 in compt := !compt + 3;
      [-x0; x1; x2] :: [x0; -x1] :: [x0; -x2] :: ((tseitin f1)@(tseitin f2)) 
  | And (f1, f2) -> let x0, x1, x2 = !compt + 1, !compt + 2, !compt + 3 in compt := !compt + 3;
      [x0; -x1; -x2] :: [-x0; x1] :: [-x0; x2] :: ((tseitin f1)@(tseitin f2)) 
  | Not (f1) -> let x0, x1 = !compt + 1, !compt + 2 in compt := !compt + 2;
      [-x0; -x1] :: [x0; x1] :: (tseitin f1)
  | Imply (f1, f2) -> tseitin (Or (Not (f1), f2))
  | Var (x) -> let x0, x1 = var x, !compt + 1 in incr compt;
      [[-x0; x1]; [x0; -x1]];;
(* Prend en entrée un arbre représentant une formule, et applique la transformation de tseitin
   pour renvoyer une formule sous forme normale conjonctive équivalente. *)

let rec string_of_clause = function
  | [] -> "0\n"
  | x :: r -> (string_of_int x)^" "^(string_of_clause r);;

let output_sat file sat = 
  let n = !compt
  and p = List.length sat in
output_string file ("p cnf "^(string_of_int n)^" "^(string_of_int p)^"\n");
  let c = ref sat in
for i = 1 to p do
  let s = string_of_clause (List.hd !c) in
  output_string file s; c := List.tl !c
done;;

let print_solution t = Assoc.mapi (fun var k -> print_string (var^" = "); print_int t.(k); print_newline()) !table;;

let main () =
let file = open_out "../Test/tseitin.cnf" in
let channel = open_in (try Sys.argv.(1) with _ -> "../Test/form0.cnf") in
let lexbuf = Lexing.from_channel channel in
let form = Parser.form Lexer.lexer lexbuf in
output_sat file (tseitin form);
flush file;
Sys.command "./../dpll ../Test/tseitin.cnf";;

main ()
