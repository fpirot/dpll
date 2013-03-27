module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Order = Order.Make (Core);;
module Wlit = Wlit.Make (Clause) (Core);;
module Oper = Oper.Make (Clause) (Core) (Order) (Wlit);;

exception TimeOut;;
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

let (set_time, get_time) =
	let t = ref 0. in
		((fun () -> t := Sys.time()),
		(fun () -> if Sys.time() -. !t > 10. then raise TimeOut))

let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

let dpll env = 
  let rec aux env =
  	get_time ();
    let (x, envtrue, envfalse) = try Oper.split env 
      with Not_found -> if Oper.is_empty env then raise Core.Satisfiable else 
	  if debug then begin
	    List.iter (fun (x,y) -> print_int x; print_string ": ";
	      List.iter (fun x -> print_list x) y;
	      print_newline()) (Oper.bindings env)
	  end;
	  raise Core.Unsatisfiable
    in
    try (
      if debug then begin
	print_string "Gamble: ";
	print_int x;
	print_newline();
      end;
      Core.write x; Oper.propag x;
      (* On assigne la valeur de x, et on rentre cette assignation dans une
	 liste pour gérer le backtrack. *)
      let env' = Oper.propagation x envtrue in
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
	let env' = Oper.propagation x envfalse in
	Oper.flush();
	aux env')
      with Core.Unsatisfiable -> (Oper.restore(); 
				    (* On annule les dernières assignations. *)
				    raise Core.Unsatisfiable)
  in Oper.init();
  (* Gère l'initialisation des structures référentes. *)
  set_time ();
  aux env;;
(* Renvoie l'exception Satisfiable dans le cas où l'instance est
   satisfiable, ou Unsatisfiable dans le cas contraire. *)

try dpll (Oper.create ()) with 
  |Core.Satisfiable -> 
    if verify Core.lst then print_string "c SATISFIABLE\n" else print_string "c ERROR\n"
  |Core.Unsatisfiable ->
    print_string "s UNSATISFIABLE\n"
  |TimeOut ->
  	print_string "s TIME OUT\n";;
