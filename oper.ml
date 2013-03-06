module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
  sig
    type map
    type cls
    val empty : map
    val create : int list -> cls
    val reset : unit -> unit
    val is_empty : map -> bool
    val are_sat : cls -> int
    val mem : int -> map -> bool
    val add : cls -> map -> map
    val variable : int -> cls -> cls
    val remove : cls -> map -> map
    val bindings : map -> (int * int list list) list
    val elements : cls -> int list
    val extract : int -> map -> cls list * map
  end;;

module type OrdElt =
  sig
    type order
    val hd : order -> int
    val tl : order -> order
  end;;



module OpCore = functor (Elt : OpElt) -> functor (Ord : OrdElt) ->
  struct
    type env = {clause: Elt.map; order: Ord.order}
    type cls = Elt.cls
    type map = Elt.map

(* Extrait une variable selon l'ordre *)
    let split env =
      let k = Ord.hd env.order in
      let (ltrue, mtrue) = Elt.extract k env.clause
      and (lfalse, mfalse) = Elt.extract (-k) env.clause in
      (k, (ltrue, {clause = mtrue; order = Ord.tl env.order}),
	  (lfalse, {clause = mfalse; order = Ord.tl env.order}))
	  
(* Ajoute une liste de clause à l'environnement *)
	  let merge lst env =
	    {clause = List.fold_left (fun x m -> if Elt.are_sat x > 0 then Elt.add x m else m) lst env.clause;
	    order = env.order}
	    
    
    let is_empty env = Elt.is_empty env.clause
     
  end;;


module type OpAbstract = functor (Elt : OpElt) -> functor (Ord : OrdElt) ->
  sig
    type env
    type set
    type map
    val split : env -> (int * (set list * env) * (set list * env))
    val assignment : int * set list -> set list option * set list option
    val is_empty : env -> bool
  end;;


module Op = (OpCore : OpAbstract);;
