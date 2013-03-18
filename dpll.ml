module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Order = Order.Make (Clause) (Core);;
module Wlit = Wlit.Make (Clause) (Core);;
module Oper = Oper.Make (Clause) (Core) (Order) (Wlit);;


let debug = false;;

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

let verify lst =
  List.for_all (fun l -> List.exists (fun x -> Core.read x = x) l) lst


let dpll env = 
  let rec aux env =
    let (x, (ltrue, envtrue), (lfalse, envfalse)) = Oper.split (Oper.update env) in
    try (
      if debug then begin
	print_string "Gamble: ";
	print_int x;
	print_newline();
      end;
      Core.write x; Oper.propag x;
      (* On assigne la valeur de x, et on rentre cette assignation dans une
	 liste pour gérer le backtrack. *)
      let env' = Oper.propagation envtrue lfalse x in
      Oper.flush();
      (* On stocke l'ensemble des assignations effectuées avec le pari
	 x, pour gérer le backtrack. *)
      aux env')
    with Core.Unsatisfiable -> 
      try (
	Oper.flush();
	(* On stocke les assignations qui étaient en cours. *)
	Oper.restore();
	(* On annule les assignations effectuées à l'étape
	   précédente. *)
	if debug then begin
	  print_string "Gamble: ";
	  print_int (-x);
	  print_newline();
	end;
	Core.write (-x); Oper.propag (-x);
	let env' = Oper.propagation envfalse ltrue (-x) in
	Oper.flush();
	aux env')
      with Core.Unsatisfiable -> (Oper.restore(); 
				    (* On annule les dernières assignations. *)
				    raise Core.Unsatisfiable)
  in Oper.init();
  (* Gère l'initialisation des structures référentes. *)
  aux env;;
(* Renvoie l'exception Satisfiable dans le cas où l'instance est
   satisfiable, ou Unsatisfiable dans le cas contraire. *)

let t = Sys.time() in
(try dpll (Oper.create ()) with 
  |Core.Satisfiable -> 
    print_string "s SATISFIABLE\nc Possible assignation: ";
    List.iter (fun x -> print_int x; print_char ' ') (valuation Core.var);
    print_newline()
  |Core.Unsatisfiable ->
    print_string "s UNSATISFIABLE\n");
print_string "c Result found within "; print_float (Sys.time() -. t); print_string " seconds.\n";;
