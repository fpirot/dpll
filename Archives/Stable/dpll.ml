module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Wlit = Wlit.Make (Core);;
module Order = Order.Make (Core) (Clause);;
module Oper = Oper.Make (Core) (Clause) (Wlit) (Order) ;;


let debug = true;;

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

let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

let dpll env = 
  let rec aux i env =
    (* i est la profondeur actuelle des paris. *)
    let (x, envtrue, envfalse) = try Oper.split env 
      with Not_found -> if Oper.is_empty env then raise Core.Satisfiable else 
	  if debug then begin
	    List.iter (fun (x,y) -> print_int x; print_string ": ";
	      List.iter (fun x -> print_list x) y;
	      print_newline()) (Oper.bindings env)
 	  end;
	raise Core.Unsatisfiable
    in
    Core.fix_depth i;
    try (
      if debug then begin
	print_string "Gamble: ";
	print_int x;
	print_newline();
      end;
      Core.write x;
      (* On assigne la valeur de x, et on rentre cette assignation dans une
	 liste pour gérer le backtrack. *)
      let env' = Oper.propagation x envtrue in
      aux (i+1) env')
      (* On va un niveau plus profond dans les paris. *)
    with Core.Unsatisfiable ->
      try (
	Core.restore i;
	(* On annule les assignations effectuées à l'étape
	   précédente. *)
	Core.fix_depth i;
	if debug then begin
	  print_string "Gamble: ";
	  print_int (-x);
	  print_newline();
	end;
	Core.write (-x);
	let env' = Oper.propagation (-x) envfalse in
	aux (i+1) env')
      with Core.Unsatisfiable -> (Core.restore i; 
				    (* On annule les dernières assignations. *)
				    raise Core.Unsatisfiable)
  in Oper.init();
  (* Gère l'initialisation des structures référentes. *)
  aux env;;
(* Renvoie l'exception Satisfiable dans le cas où l'instance est
   satisfiable, ou Unsatisfiable dans le cas contraire. *)

let t = Sys.time() in
(try dpll 0 (Oper.create ()) with 
  |Core.Satisfiable -> 
    print_string "s SATISFIABLE\nc Possible assignation: ";
    List.iter (fun x -> print_int x; print_char ' ') (valuation Core.var);
    print_newline();
    if verify Core.lst then print_string "c Assignation verified with success.\n" else print_string "c Error during verification.\n"
  |Core.Unsatisfiable ->
    print_string "s UNSATISFIABLE\n");
print_string "c Result found within "; print_float (Sys.time() -. t); print_string " seconds.\n";;
