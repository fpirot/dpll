module Core = Core.Make;;
module Clause = Clause.Make (Core);;
module Wlit = Wlit.Make (Core);;
module Graph = Graph.Make (Core);;
module Order = Order.Make (Core) (Clause);;
module Oper = Oper.Make (Core) (Clause) (Wlit) (Order) (Graph);;

exception TimeOut;;
let debug = false;;

let (set_time, get_time) =
  let t = ref 0. in
  ((fun () -> t := Sys.time()),
   (fun () -> if Sys.time() -. !t > 10. then raise TimeOut))

let print_graph () = ();;

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
  let rec aux i env =
    get_time (); 
    if i = 0 then begin
      let nb_cls = Core.nb_cls () in
      Core.restore 0;
      Core.fix_depth 0;
      let env' = try Oper.propagation (new_cls 0) (Oper.update nb_cls env) channel
	with _ -> raise (Core.Unsatisfiable (-1)) in
      try aux 1 env'
      with Oper.Backtrack 0 -> aux 0 env'
    end else begin
      if Oper.is_empty env then raise Core.Satisfiable
      else let x = Oper.extract env in
	   let nb_cls = Core.nb_cls () in
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
  in Oper.init();
  (* Gère l'initialisation des structures référentes. *)
  if Oper.is_empty env then raise Core.Satisfiable
  else aux 0 env;;
(* Renvoie l'exception Satisfiable dans le cas où l'instance est
   satisfiable, ou Unsatisfiable dans le cas contraire. *)


try dpll (Oper.create ()) with 
  |Core.Satisfiable -> 
    if verify Core.lst then print_string "c SATISFIABLE\n" else print_string "c ERROR\n"
  |Core.Unsatisfiable -1 ->
    print_string "s UNSATISFIABLE\n"
  |TimeOut ->
    print_string "s TIME OUT\n";;
