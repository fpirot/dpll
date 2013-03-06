module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Oper = Oper.Make (Clause) (Core);;

exception Unsatisfiable;;

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
	Core.write x; Oper.propag x;
	let env' = Oper.propagation envtrue lfalse in
	aux env')
      with Unsatisfiable -> 
	try (
	Oper.restore();
	Core.write (-x); Oper.propag (-x);
	let env' = Oper.propagation envfalse ltrue in
	aux env')
	with Unsatisfiable -> (Oper.restore(); raise Unsatisfiable)
    end;
  in aux env; valuation Core.var;;
  (* Renvoie une assignation qui permet de satisfaire l'instance
     d'entrée, ou l'exception Unsatisfiable si cette dernière n'est pas
     satisfiable. *)

  dpll (Oper.create ());;
