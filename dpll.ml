module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Wlit = Wlit.Make (Core);;
module Graph = Graph.Make (Core);;
module Order = Order.Make (Core) (Clause);;
module Oper = Oper.Make (Core) (Clause) (Wlit) (Order) (Graph);;


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

let print_list l =
  let rec print = function
    |[] -> print_string "]"
    |[a] -> print_int a; print_string "]"
    |a::l -> print_int a; print_string "; "; print l in
  print_string "["; print l

let dpll env =
  let channel = open_out "log" in
  let rec aux i x env =
    (* i est la profondeur actuelle des paris. *)
    let nb_cls = Core.nb_cls () in
    try (
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
      let env' = Oper.propagation x (Oper.update x nb_cls env) channel in
	if Oper.is_empty env' then raise Core.Satisfiable
	else let x' = Oper.extract env' in
	     aux (i+1) x' env')

    with Oper.Backtrack (k,x') -> if k = i then
	let y = if x' = 0 then (-x) else (-x') in aux i y env
      else raise (Oper.Backtrack (k,x'))

  in Oper.init();
  (* Gère l'initialisation des structures référentes. *)
  if Oper.is_empty env then raise Core.Satisfiable
  else aux 0 (Oper.extract env) env;;
(* Renvoie l'exception Satisfiable dans le cas où l'instance est
   satisfiable, ou Unsatisfiable dans le cas contraire. *)

(*
let t = Sys.time() in
(try dpll (Oper.create ()) with 
  |Core.Satisfiable ->
    print_string "s SATISFIABLE\nc Possible assignation: ";
    List.iter (fun x -> print_int x; print_char ' ') (valuation Core.var);
    print_newline();
    if verify Core.lst then print_string "c Assignation verified with success.\n" else print_string "c Error during verification.\n"
  |_ ->
    print_string "s UNSATISFIABLE\n");
print_string "c Result found within "; print_float (Sys.time() -. t); print_string " seconds.\n";;*)


(try dpll (Oper.create ()) with
  |Core.Satisfiable -> 
    if verify Core.lst then print_string "s SATISFIABLE\n" else print_string "s ERROR.\n"
  |_ ->
    print_string "s UNSATISFIABLE\n");;
