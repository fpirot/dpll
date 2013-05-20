module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Wlit = Wlit.Make (Core);;
module Graph = Graph.Make (Core);;
module Proof = Proof.Make (Core);;
module Order = Order.Make (Core);;
module Oper = Oper.Make (Core) (Clause) (Wlit) (Order) (Graph) (Proof);;
module Smt = struct
  let validity () = []
end;;

exception Unsatisfiable;;

let file = try open_out "Test/result.txt" with _ -> open_out "../Test/result.txt";;

let debug = false;;

let rec new_cls n = if n = Core.nb_cls() then [] else n :: (new_cls (n+1));;

let rec valuation n =
  let rec aux l = function
    | 0 -> l
    | n -> let v = Core.read n in
	   if v = 0 then aux (n :: l) (n-1)
	   else aux (v :: l) (n-1)
  in aux [] n;;
(* Une fois que l'on a fini d'assigner les variables et que
   l'instance d'entrée est satisfaite, renvoie une liste
   d'assignations qui satisfait le problème, à partir des données
   entrées dans Assig. n est le nombre de variables du problème. *)

let write_result file b =
  if b then begin
    output_string file "SATISFIABLE\n";
    output_string file "var "; output_string file (string_of_int Core.var); output_string file "\n";
    let l = valuation Core.var in
    List.iter (fun x -> output_string file (string_of_int x); output_string file " ") l
  end else output_string file "UNSATISFIABLE\n";
  flush file;;
(* Ecrit la valuation trouvée par dpll sur le fichier. *)

let verify lst = List.for_all (fun l -> List.exists (fun x -> Core.read x = x) l) lst;;

let print_list l =
  let rec print = function
    |[] -> print_string "]"
    |[a] -> print_int a; print_string "]"
    |a::l -> print_int a; print_string "; "; print l in
  print_string "["; print l;;

let dpll env =
  let channel = open_out "log" in  
  let rec aux i env =
    (* i est la profondeur actuelle des paris. *)
    let nb_cls = Core.nb_cls () in
    if i = 0 then begin
      Core.restore 0;
      Core.fix_depth 0;
      try propag (new_cls 0) env nb_cls 0 with Oper.Backtrack(0) -> raise Unsatisfiable
    end else begin
      if Oper.is_empty env then begin
	if Core.smt then begin
	  (* On rentre dans la partie vérification de concordance avec la théorie*)
	  write_result file true;
	  (* On génère la clause insatisfiable selon la théorie *)
	  let c = Smt.validity () in
	  if c = [] then raise Core.Satisfiable
	  else Oper.add_cls c
	end;
	raise Core.Satisfiable
      end      
      else let x = Oper.extract env in
	   Core.restore i;
	   Core.fix_depth i;
	   if debug then begin
	     print_string "Gamble: ";
	     print_int x;
	     print_newline();
	   end;
	   Core.write x;
	   (* On assigne la valeur de x, et on rentre cette assignation
	      dans une liste pour gérer le backtrack. *)
	   propag (Oper.find x env) (Oper.remove x env) nb_cls i
    end
  and propag l e nb_cls i =
    Core.restore (i+1);
    (* On supprime les assignations des niveaux plus hauts
       que celui actuel. *)
    Core.fix_depth i;
    let e' = Oper.propagation l (Oper.update nb_cls e) channel in
    try aux (i+1) e'
    with Oper.Backtrack k -> if k = i then
	let l' = new_cls nb_cls in propag l' e' (Core.nb_cls()) i
      else raise (Oper.Backtrack k)
  in if Oper.is_empty env then raise Core.Satisfiable
    else aux 0 env;;
(* Renvoie l'exception Satisfiable dans le cas où l'instance est
   satisfiable, ou Unsatisfiable dans le cas contraire. *)


let t = Sys.time() in
(try dpll (Oper.create ())
 with 
   |Core.Satisfiable -> write_result file true;
     if Core.aff then (
       print_string "s SATISFIABLE\nc Possible assignation: ";
       List.iter (fun x -> print_int x; print_char ' ') (valuation Core.var);
       print_newline();
       if verify Core.lst then print_string "c Assignation verified with success.\n" else print_string "c Error during verification.\n")
   |Unsatisfiable -> write_result file false;
    (* On tombe sur cette exception lorsque la propagation au niveau 0
       tombe sur une incohérence. *)
     if Core.aff then print_string "UNSATISFIABLE\n");
if Core.aff then (print_string "c Result found within "; print_float (Sys.time() -. t); print_string " seconds.\n");;
