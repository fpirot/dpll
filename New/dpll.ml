exception Unsatisfiable;;

module type Assig =
  sig
    val nbr : int
    val read : int -> int
    val write : int -> int -> unit
  end;;

module type Op =
  sig
    type env
    type cls
    val is_empty : env -> bool
    val split : env -> int * (cls list * env) * (cls list * env)
    val propagation : env -> cls list -> env
  end;;



module Dpll = functor (Op : Op) -> functor (Assig : Assig) ->
struct 
  let rec valuation n =
    let rec aux l = function
      | 0 -> l
      | n -> let v = Assig.read n in
	     if v = 0 then aux (n :: l) (n-1)
	     else aux (v :: l) (n-1)
    in aux [] n
(* Une fois que l'on a fini d'assigner les variables et que l'instance d'entrée est satisfaite, renvoie une liste d'assignations qui satisfait le problème, à partir des données entrées dans Assig. n est le nombre de variables du problème. *)

  let dpll env = 
    let rec aux env =
      if Op.is_empty env then ()
      else begin
	let (x, (ltrue, envtrue), (lfalse, envfalse)) = Op.split env in
	try (
	  Assig.write x x; 
	  let env' = Op.propagation envtrue lfalse in
	  aux env')
	with Unsatisfiable -> (
	  Assig.write x (-x);
	  let env' = Op.propagation envfalse ltrue in
	  aux env')
      end;
    in aux env; valuation Assig.nbr
(* Renvoie une assignation qui permet de satisfaire l'instance d'entrée, ou l'exception Unsatisfiable si cette dernière n'est pas satisfiable. *)
  
end;;
