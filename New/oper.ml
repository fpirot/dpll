(* Module d'operation sur les clauses propres a DPLL *)

module type CoreElt =
  sig
    type order
    val hd : order -> int
    val tl : order -> order
    val read : int -> int
    val write : int -> int -> unit
  end;;


module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
  sig
    type cls
    type map
    val empty : map
    val is_empty : map -> bool
    val are_sat : cls -> int
    val find : int -> map -> cls list
    val choose : cls -> int
    val add : cls -> map -> map (* Ajoute une clause a la map *)
    val bindings : map -> (int * int list list) list (* Une fonction d'affichage *)
    val extract : int -> map -> cls list * map (* Extrait la liste des clauses comportant une variable et renvoie la map privee de ces clauses *)
  end ;;



module OpCore = functor (Elt : OpElt) -> functor (Cor : CoreElt) ->
  struct
    
    exception Satisfiable;;
    exception Unsatisfiable;;
    
    type env = {clause: Elt.map; order: Cor.order}
    type cls = Elt.cls
    type map = Elt.map

    module St = Set.Make (
      struct
	type t = int
	let compare = compare
      end)

(* Extrait une variable selon l'ordre *)
    let split env =
      let k = Cor.hd env.order in
      let (ltrue, mtrue) = Elt.extract k env.clause
      and (lfalse, mfalse) = Elt.extract (-k) env.clause in
      (k, (ltrue, {clause = mtrue; order = Cor.tl env.order}),
	  (lfalse, {clause = mfalse; order = Cor.tl env.order}))
    
    let is_empty env = Elt.is_empty env.clause

    let select lc setv = 	
      List.fold_right (fun c s -> let n = Elt.are_sat c in
				  if n = 0 then raise Unsatisfiable
				  else if n = 1 then 
				    let x = Elt.choose c in St.add x s
				  else s) lc setv

    let rec propagation env lc =
      let rec aux env lc setv =
	let setv' = select lc setv in
	if St.is_empty setv' then env
	else begin
	  let x = St.choose setv' in
	  Cor.write (abs x) x;
	  let (_, m) = Elt.extract x env.clause in
	  let lc' = Elt.find (-x) env.clause in
	  aux {clause = m; order = Cor.tl env.order} lc' setv'
	end
      in aux env lc St.empty

  end;;


module type OpAbstract = functor (Elt : OpElt) -> functor (Cor : CoreElt) ->
  sig
    exception Satisfiable
    exception Unsatisfiable
    type env
    type cls
    type map
    val split : env -> (int * (cls list * env) * (cls list * env))
    val is_empty : env -> bool
    val propagation : env -> cls list -> env
  end;;


module Make = (OpCore : OpAbstract);;