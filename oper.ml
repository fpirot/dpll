(* Module d'operation sur les clauses propres a DPLL *)

module type CoreElt =
sig
  exception Satisfiable
  type cls = int
  exception Unsatisfiable of cls
  val nb_cls : unit -> int
  val var : int
  val wlit : bool
  val lst : int list list
  val read : int -> int
  val write : ?father: cls -> int -> unit
  val father : int -> cls
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val heur : string
  val ord : (int * int) list
  val is_singleton : cls -> int
  val choose : cls -> int
  val literals : cls -> int list
  val cls_make : int -> cls
  val length : cls -> int
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val backtrack : cls -> int
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
  val add : cls -> map -> map
  val is_empty : map -> bool
end;;

module type Order =
sig
  type order
  type map
  type cls
  val is_empty : order -> bool
  val create : unit -> order
  val add : cls -> order -> order
  val extract : map -> order -> int
  val update : int -> map -> order -> order
end;;

module type WlitElt =
sig
  type cls
  type set
  type setc
  val update : int -> set * setc
  val init : unit -> unit
  val update_cls : int -> unit
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
  type cls
  type graph
  val create : unit -> graph
  val add : int -> cls -> graph -> graph
  val find : int -> int -> graph -> int
end;;

module OpCore = functor (Cor : CoreElt) -> functor (Elt : OpElt with type cls = Cor.cls) 
    -> functor (Wlit: WlitElt with type cls = Cor.cls)
        -> functor (Ord: Order with type map = Elt.map and type cls = Cor.cls)
	    -> functor (Graph : Graph with type cls = Cor.cls) ->
struct

  type env = {clause: Elt.map; order: Ord.order}
  type cls = Cor.cls
  type set = Wlit.set

  exception Backtrack of int

  let debug = true
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
  let extract env = Ord.extract env.clause env.order

  let update x n env = 
    let p = Cor.nb_cls () in
    if Cor.wlit then Wlit.update_cls n;
    let map = ref env.clause and ord = ref env.order in
    for i = n to p-1 do
      map := Elt.add i !map; ord := Ord.add i !ord done;
{clause = Elt.extract x !map; order = Ord.update x !map !ord}
   
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
				    print_string ", because of ";
				    print_list (Cor.literals c);
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
      let g' = Graph.add x (Cor.father x) g 
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
      let g' = Graph.add x (Cor.father x) g
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
    try
      if Cor.wlit then wlit_propagation x env
      else simple_propagation x env
    with Cor.Unsatisfiable c ->
      let i = Cor.backtrack c in
      raise (Backtrack i)

  let bindings env = Elt.bindings env.clause

  let init () = if Cor.wlit then Wlit.init () else ()

end;;


module type OpAbstract = functor (Cor : CoreElt) -> functor (Elt : OpElt with type cls = Cor.cls) 
    -> functor (Wlit: WlitElt with type cls = Cor.cls) 
        -> functor (Ord: Order with type map = Elt.map and type cls = Cor.cls) 
	    -> functor (Graph: Graph with type cls = Cor.cls) ->
sig
  type env
  type cls
  type set
  exception Backtrack of int
  val create : unit -> env
  val is_empty : env -> bool
  val extract : env -> int
  val update : int -> int -> env -> env
  val entail : int -> env -> set
  val propagation : int -> env -> env
  val bindings : env -> (int * int list list) list
  val init : unit -> unit
end;;

module Make = (OpCore : OpAbstract);;
