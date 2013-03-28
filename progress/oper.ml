
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
  val reset : int -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val heur : string
  val ord : (int * int) list
end;;

module type OpElt = functor (Elt: CoreElt) ->
(* Module qui référencie l'ensemble des clauses du problème. *)
sig
  type cls = Elt.cls
  type map
  val empty : map
  val create : int list list -> map
  val is_singleton : cls -> int
  val find : int -> map -> cls list
  val choose : cls -> int
  val cls_make : int -> cls
  val bindings : map -> (int * int list list) list
  val extract : int -> map -> map
  val remove : cls -> map -> map
  val is_empty : map -> bool
  val length : cls -> int
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;

module type Order = functor (Cor: CoreElt) -> functor (Elt: OpElt) ->
sig
  type order
  val is_empty : order -> bool
  val create : unit -> order
  val extract : Elt(Cor).map -> order -> int * order
  val update : int -> Elt(Cor).map -> order -> order
end;;

module type WlitElt = functor (Elt: CoreElt) ->
sig
  type cls = Elt.cls
  type set
  type setc
  val update : int -> set * setc
  val init : unit -> unit
  val fold : (cls -> 'a -> 'a) -> setc -> 'a -> 'a
  val choose : set -> int
  val singleton : int -> set
  val empty : set
  val is_empty : set -> bool
  val union : set -> set -> set
  val add : int -> set -> set
  val remove : int -> set -> set
end;;

module OpCore = functor (OpElt : OpElt) -> functor (Cor : CoreElt) -> functor (Order: Order) -> functor (WlitElt: WlitElt) ->
struct

  module Elt = OpElt (Cor)
  module Ord = Order (Cor) (OpElt)
  module Wlit = WlitElt (Cor)

  type env = {clause: Elt.map; order: Ord.order}
  type cls = Elt.cls

  let debug = false
  let print_list l=
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

  let entail x env =
    List.fold_right (fun c s -> let x = Elt.is_singleton c in
				if x <> 0 then (
				  if debug then begin
				    print_string "Select: ";
				    print_int x;
				    print_newline()
				  end;	  
				  Cor.write ~father:c x; Wlit.add x s)
				else s) (Elt.find (-x) env.clause) Wlit.empty
  (* Sélectionne dans une liste de clauses celles qui sont des
     singletons, et renvoie l'union de leurs éléments. On renvoie
     ainsi un ensemble de nouvelles assignations contraintes par
     celle en cours. *)

  let simple_propagation x env =
    let rec aux env x setv =
      let set_entailed = entail x env in
      let setv' = Wlit.union set_entailed setv in
      if Wlit.is_empty setv' then env
      (* Lorsqu'on n'a plus d'assignations contraintes, la propagation
	 s'arrête. On rentre la liste des assignations effectuée au cours de
	 cette propagation dans une liste, et on passe au prochain pari. *)
      else begin
	let x = Wlit.choose setv' in
	if debug then begin
	  print_string "Choice: ";
	  print_int x;
	  print_newline()
	end;
	let ord = Ord.update x env.clause env.order in
	let setv' = Wlit.remove x setv'
	and m = Elt.extract x env.clause in
	aux {clause = m; order = ord} x setv'
      end
    in aux env x Wlit.empty

  let wlit_propagation x env =
    let rec aux env setv = 
      if Wlit.is_empty setv then env
      else let x = Wlit.choose setv in
	   if debug then begin
             print_string "WLit choose: ";
             print_int x;
             print_newline()
           end;
	   let ord = Ord.update x env.clause env.order
	   and setv = Wlit.remove x setv in
	   let (sbord, ssat) = Wlit.update x in
	   let setv' = Wlit.union sbord setv in
	   aux {clause = Wlit.fold (fun c m -> Elt.remove (Elt.cls_make c) m) ssat env.clause ; order = ord} setv'
    in aux env (Wlit.singleton x)

    
  let propagation x env =
    if Cor.wlit then wlit_propagation x env
    else simple_propagation x env
    
  let bindings env = Elt.bindings env.clause
(*
  let update env = {clause = env.clause; order = Ord.update 0 0 env.order}
*)
  let init () = if Cor.wlit then Wlit.init () else ()

end;;


module type OpAbstract = functor (Elt: OpElt) -> functor (Cor: CoreElt) -> 
  functor (Ord: Order) -> functor (Wlit: WlitElt) ->

sig
  type env
  type cls
  val create : unit -> env
  val is_empty : env -> bool
  val split : env -> (int * env * env)
  val entail : int -> env -> Wlit(Cor).set
  val propagation : int -> env -> env
  val bindings : env -> (int * int list list) list
  val init : unit -> unit
end;;

module Make = (OpCore : OpAbstract);;
