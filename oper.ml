
(* Module d'operation sur les clauses propres a DPLL *)

module type CoreElt =
sig
  exception Satisfiable
  exception Unsatisfiable 
  val wlit : bool
  val lst : int list list
  val read : int -> int
  val write : int -> unit
  val reset : int -> unit
end;;

module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
sig
  type cls
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

module type Order = functor (Elt: OpElt) ->
sig
  type order
  val is_empty : order -> bool
  val create : unit -> order
  val extract : Elt.map -> order -> int * order
  val update : int -> Elt.map -> order -> order
end;;

module type WlitElt =
sig
  type set
  val update : int -> set * set
  val init : unit -> unit
  val fold : (int -> 'a -> 'a) -> set -> 'a -> 'a
  val choose : set -> int
  val singleton : int -> set
  val empty : set
  val is_empty : set -> bool
  val union : set -> set -> set
  val add : int -> set -> set
  val remove : int -> set -> set
end;;

module OpCore = functor (Elt : OpElt) -> functor (Cor : CoreElt) -> functor (Order: Order) -> functor (Wlit: WlitElt) ->
struct

  module Ord = Order (Elt)

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



  let (propag, flush, restore, reset) = 
    let lst = ref []
    and ls = ref [] in
    ((fun x -> ls := x::(!ls)),
     (fun () -> lst := (!ls)::(!lst); ls := [];
       if debug then begin
	 print_string "Flush: ";
	 List.iter (fun l -> print_list l) (!lst);
	 print_newline()
       end;),
     (fun () ->  
       if debug then begin
	 print_string "Restore: ";
	 List.iter (fun l -> print_list l) (!lst);
	 print_newline()
       end;
       List.iter (fun x -> Cor.reset x)
	 (try List.hd (!lst) with Failure _ -> raise Cor.Unsatisfiable);
       lst := List.tl (!lst)),
     (fun () -> ls := []; lst := []))

(* Extrait une variable selon l'ordre *)
let split env =
  let k, ord = Ord.extract env.clause env.order in
  let mtrue = Elt.extract k env.clause
  and mfalse = Elt.extract (-k) env.clause in
  (k, {clause = mtrue; order = ord}, {clause = mfalse; order = Ord.update (-k) env.clause env.order})
	 
  let is_empty env = Elt.is_empty env.clause

  let engendre x env =
    List.fold_right (fun c s -> let x = Elt.is_singleton c in
				if x <> 0 then (
				  if debug then begin
				    print_string "Select: ";
				    print_int x;
				    print_newline()
				  end;
				  Wlit.add x s)
				else s) (Elt.find (-x) env.clause) Wlit.empty
  (* Sélectionne dans une liste de clauses celles qui sont des
     singletons, et renvoie l'union de leurs éléments. On renvoie
     ainsi un ensemble de nouvelles assignations contraintes par
     celle en cours. *)

  let simple_propagation x env =
    let rec aux env x setv =
      let setv' = Wlit.union (engendre x env) setv in
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
	Cor.write x; propag x;
	(* On assigne x à vrai, et on rentre cette assignation dans
	   une liste, afin de désassigner convenablement lors du
	   potentiel backtrack. *)
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
	   Cor.write x; propag x;
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
  val propag : int -> unit
  val flush : unit -> unit
  val restore : unit -> unit
  val reset : unit -> unit
  val split : env -> (int * env * env)
  val is_empty : env -> bool
  val propagation : int -> env -> env
  val bindings : env -> (int * int list list) list
  val init : unit -> unit
end;;

module Make = (OpCore : OpAbstract);;
