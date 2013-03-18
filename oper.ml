
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

module type OrderElt =
sig
  type order
  val create : unit -> order
  val extract : order -> int * order
  val is_empty : order -> bool
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
  val clause : int -> cls
  val bindings : map -> (int * int list list) list
  val extract : int -> map -> cls list * map
  val remove : cls -> map -> map
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

module OpCore = functor (Elt : OpElt) -> functor (Cor : CoreElt) -> functor (Ord : OrderElt) -> functor (Wlit: WlitElt) ->
struct
  
  type env = {clause: Elt.map; order: Ord.order}
  type cls = Elt.cls

  let debug = false
  let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  let create () = 
    {clause = Elt.create Cor.lst; order = Ord.create ()}



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

  (* choisit la variable sur laquelle faire le prochain pari, en
     fonction de l'heuristique en cours. *)
  let rec choice ord =
    if Ord.is_empty ord then raise Cor.Satisfiable
    else let (x, order) = Ord.extract ord in
	 if Cor.read x = 0 then x else choice order

(* Extrait une variable selon l'ordre *)
let split env =
  let (k, ord) = Ord.extract env.order in
  let (ltrue, mtrue) = Elt.extract k env.clause
  and (lfalse, mfalse) = Elt.extract (-k) env.clause in
  (k, (ltrue, {clause = mtrue; order = ord}),
   (lfalse, {clause = mfalse; order = ord}))
	 
  let is_empty env = Ord.is_empty env.order

  let select lc setv =
    List.fold_right (fun c s -> let x = Elt.is_singleton c in
				if x <> 0 then (
				  if debug then begin
				    print_string "Select: ";
				    print_int x;
				    print_newline()
				  end;
				  Wlit.add x s)
				else s) lc setv
  (* Sélectionne dans une liste de clauses celles qui sont des
     singletons, et renvoie l'union de leurs éléments. On renvoie
     ainsi un ensemble de nouvelles assignations contraintes par
     celle en cours. *)

  let simple_propagation env lc =
    let rec aux env lc setv =
      let setv' = select lc setv in
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
	Cor.write x; propag x;
	(* On assigne x à vrai, et on rentre cette assignation dans
	   une liste, afin de désassigner convenablement lors du
	   potentiel backtrack. *)
	let setv' = Wlit.remove x setv'
	and (_, m) = Elt.extract x env.clause
	and lc' = Elt.find (-x) env.clause in
	aux {clause = m; order = env.order} lc' setv'
      end
    in aux env lc Wlit.empty

  let wlit_propagation env lc x =
    let rec aux env setv = 
      if Wlit.is_empty setv then env
      else let x = Wlit.choose setv in
	   if debug then begin
             print_string "WLit choose: ";
             print_int x;
             print_newline()
           end;
	   let setv = Wlit.remove x setv in
	   Cor.write x; propag x;
	   let (sbord, ssat) = Wlit.update x in
	   let setv' = Wlit.union sbord setv in
	   aux {clause = Wlit.fold (fun c m -> Elt.remove (Elt.clause c) m) ssat env.clause ; order = env.order} setv'
    in aux env (Wlit.singleton x)

    
  let propagation env lc x =
    if Cor.wlit then wlit_propagation env lc x
    else simple_propagation env lc

    
  let bindings env = Elt.bindings env.clause
(*
  let update env = {clause = env.clause; order = Ord.update 0 0 env.order}
*)
  let init () = if Cor.wlit then Wlit.init () else ()

end;;


module type OpAbstract = functor (Elt : OpElt) -> functor (Cor : CoreElt) -> functor (Ord : OrderElt) -> functor (Wlit: WlitElt) ->

sig
  type env
  type cls
  val create : unit -> env
  val propag : int -> unit
  val flush : unit -> unit
  val restore : unit -> unit
  val reset : unit -> unit
  val split : env -> (int * (cls list * env) * (cls list * env))
  val is_empty : env -> bool
  val propagation : env -> cls list -> int -> env
  val bindings : env -> (int * int list list) list
  val init : unit -> unit
end;;

module Make = (OpCore : OpAbstract);;
