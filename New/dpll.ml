module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Oper = Oper.Make (Clause) (Core);;

exception Satisfiable;;
let debug = true;;


let rec valuation n =
    let rec aux l = function
      | 0 -> l
      | n -> let v = Core.read n in
	     if v = 0 then aux (n :: l) (n-1)
	     else aux (v :: l) (n-1)
    in aux [] n;;
  (* Une fois que l'on a fini d'assigner les variables et que l'instance
     d'entrée est satisfaite, renvoie une liste d'assignations qui
     satisfait le problème, à partir des données entrées dans Assig. n est
     le nombre de variables du problème. *)

let dpll env = 
  let rec aux env =
    if Oper.is_empty env then ()
    else begin
      let (x, (ltrue, envtrue), (lfalse, envfalse)) = Oper.split (Oper.update env) in
      try (
	if debug then begin
	  print_string "Gamble: ";
	  print_int x;
	  print_newline();
	end;
	Core.write x; Oper.propag x;
	let env' = Oper.propagation envtrue lfalse in
	Oper.flush();
	aux env')
      with Clause.Unsatisfiable -> 
	try (
	  Oper.flush();
	  Oper.restore();
	  if debug then begin
	    print_string "Gamble: ";
	    print_int (-x);
	    print_newline();
	  end;
	  Core.write (-x); Oper.propag (-x);
	  let env' = Oper.propagation envfalse ltrue in
	  Oper.flush();
	  aux env')
	with Clause.Unsatisfiable -> (Oper.restore(); raise Clause.Unsatisfiable)
    end;
  in aux env;;
  (* Renvoie une assignation qui permet de satisfaire l'instance
     d'entrée, ou l'exception Unsatisfiable si cette dernière n'est pas
     satisfiable. *)

try dpll (Oper.create ()) with 
  |Clause.Satisfiable -> 
    print_string "L'instance est satisfiable, voilà une assignation des variables possible :\n";
    List.iter (fun x -> print_int x; print_char ' ') (valuation Core.var);
    print_newline();
  |Clause.Unsatisfiable ->
    print_string "L'instance n'est pas satisfiable.\n"
