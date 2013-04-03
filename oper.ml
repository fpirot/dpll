(* Module d'operation sur les clauses propres a DPLL *)

module type CoreElt =
sig
  exception Satisfiable
  exception Unsatisfiable
  type cls = int
  val var : int
  val cls : int
  val wlit : bool
  val lst : int list list
  val read : int -> int
  val write : ?father: cls -> int -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val heur : string
  val ord : (int * int) list
  val is_singleton : cls -> int
  val choose : cls -> int
  val cls_make : int -> cls
  val length : cls -> int
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;

module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
sig
  type cls
  type map
  val empty : map
  val create : int list list -> map
  val find : int -> map -> cls list
  val bindings : map -> (int * int list list) list
  val extract : int -> map -> map
  val remove : cls -> map -> map
  val is_empty : map -> bool
end;;

module type Order =
sig
  type order
  type map
  val is_empty : order -> bool
  val create : unit -> order
  val extract : map -> order -> int * order
  val update : int -> map -> order -> order
end;;

module type WlitElt =
sig
  type cls
  type set
  type setc
  val update : int -> set * setc
  val init : unit -> unit
  val fold : (cls -> 'a -> 'a) -> setc -> 'a -> 'a
  val choose : set -> (int * cls)
  val singleton : int -> set
  val empty : set
  val is_empty : set -> bool
  val union : set -> set -> set
  val add : (int * cls) -> set -> set
  val remove : int -> set -> set
  val elements : set -> (int * cls) list
end;;

module type Graph =
sig
  type graph
  val create : unit -> graph
  val add : int -> int list -> graph -> graph
  val find : int -> int -> graph -> int
end;;

module OpCore = functor (Cor : CoreElt) -> functor (Elt : OpElt with type cls = Cor.cls) 
    -> functor (Wlit: WlitElt with type cls = Cor.cls) -> functor (Ord: Order with type map = Elt.map)
	-> functor (Graph : Graph) ->
struct

  type env = {clause: Elt.map; order: Ord.order}
  type cls = Cor.cls
  type set = Wlit.set

  let debug = false
  let print_list l =
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  let create () = let m = Elt.create Cor.lst in
    let ord = Ord.create() in
    {clause = m; order = ord}

(* Extrait une variable selon l'ordre *)
let split env =
  let k, ord = Ord.extract env.clause env.order in
  let mtrue = Elt.extract k env.clause
  and mfalse = Elt.extract (-k) env.clause in
  (k, {clause = mtrue; order = ord}, {clause = mfalse; order = Ord.update (-k) env.clause env.order})
	 
  let is_empty env = Elt.is_empty env.clause


(* ***************************************************** *)
(*       Gestion de la propagation des contraintes       *)
(* ***************************************************** *)

  let entail x env =
    List.fold_right (fun c s -> let x = Cor.is_singleton c in
				if x <> 0 then (
				  if debug then begin
				    print_string "Select: ";
				    print_int x;
				    print_newline()
				  end;	  
				  Wlit.add (x, c) s)
				else s) (Elt.find (-x) env.clause) Wlit.empty
  (* Sélectionne dans une liste de clauses celles qui sont des
     singletons, et renvoie l'union de leurs éléments. On renvoie
     ainsi un ensemble de nouvelles assignations contraintes par celle
     en cours. *)

  let simple_propagation x env =
    let rec aux env x setv g =
      let sbord = entail x env in
      let g' = Graph.add x (List.map fst (Wlit.elements sbord)) g 
      and setv' = Wlit.union sbord setv in
      if Wlit.is_empty setv' then env
      (* Lorsqu'on n'a plus d'assignations contraintes, la propagation
	 s'arrête. On rentre la liste des assignations effectuée au
	 cours de cette propagation dans une liste, et on passe au
	 prochain pari. *)
      else begin
	let (x, c) = Wlit.choose setv' in
	if debug then begin
	  print_string "Choice: ";
	  print_int x;
	  print_newline()
	end;
	Cor.write ~father:c x; 
	let ord = Ord.update x env.clause env.order
	and setv' = Wlit.remove x setv'
	and m = Elt.extract x env.clause in
	aux {clause = m; order = ord} x setv' g'
      end
    in aux env x Wlit.empty (Graph.create ())

  let wlit_propagation x env =
    let rec aux env x setv g = 
      let (sbord, ssat) = Wlit.update x in
      let g' = Graph.add x (List.map fst (Wlit.elements sbord)) g
      and setv' = Wlit.union sbord setv in
      if Wlit.is_empty setv' then env
      else begin
	let (x, c) = Wlit.choose setv' in
	if debug then begin
          print_string "WLit choose: ";
          print_int x;
          print_newline()
        end;
	Cor.write ~father:c x;
	let ord = Ord.update x env.clause env.order
	and setv' = Wlit.remove x setv'
	and m = Elt.extract x env.clause in
	aux {clause = m ; order = ord} x setv' g'
      end
    in aux env x Wlit.empty (Graph.create ())

  let propagation x env =
    if Cor.wlit then wlit_propagation x env
    else simple_propagation x env


  let bindings env = Elt.bindings env.clause

  let init () = if Cor.wlit then Wlit.init () else ()

end;;


module type OpAbstract = functor (Cor : CoreElt) -> functor (Elt : OpElt with type cls = Cor.cls) 
    -> functor (Wlit: WlitElt with type cls = Cor.cls) -> functor (Ord: Order with type map = Elt.map)
	-> functor (Graph: Graph) ->
sig
  type env
  type cls
  type set
  val create : unit -> env
  val is_empty : env -> bool
  val split : env -> (int * env * env)
  val entail : int -> env -> set
  val propagation : int -> env -> env
  val bindings : env -> (int * int list list) list
  val init : unit -> unit
end;;

module Make = (OpCore : OpAbstract);;
