
(* Module d'operation sur les clauses propres a DPLL *)

module type CoreElt =
  sig
    type order
    val lst : int list list
    val ord : order
    val hd : order -> int
    val tl : order -> order
    val read : int -> int
    val write : int -> unit
    val reset : int -> unit
    val update : order -> order
  end;;


module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
  sig
    type cls
    type map
    val empty : map
    val create : int list list -> map
    val is_empty : map -> bool
    val are_sat : cls -> int
    val find : int -> map -> cls list
    val choose : cls -> int
    val bindings : map -> (int * int list list) list
    val extract : int -> map -> cls list * map 
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

    let debug = true

    let create () = 
      {clause = Elt.create Cor.lst; order = Cor.ord}



		let (propag, flush, restore, reset) = 
	    let lst = ref []
	    and ls = ref [] in
	      ((fun x -> ls := x::(!ls)),
	      (fun () -> lst := (!ls)::(!lst); ls := []),
	      (fun () -> List.iter (fun x -> Cor.reset x) (List.hd (!lst)); lst := List.tl (!lst)),
	      (fun () -> ls := []; lst := []))

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
				  if n = 0 then  raise Unsatisfiable
				  else if n = 1 then 
				    let x = Elt.choose c in
				    if debug then begin
				      print_string "Choice: ";
				      print_int x;
				      print_newline()
				    end;
				    St.add x s
				  else s) lc setv
    (* Sélectionne dans une liste de clauses celles qui sont des
       singletons, et renvoie l'union de leurs éléments. On renvoie
       ainsi un ensemble de nouvelles assignations contraintes par
       celle en cours. *)

    let rec propagation env lc =
      let rec aux env lc setv =
	let setv' = select lc setv in
	if St.is_empty setv' then (Oper.flush (); env)
	(* Lorsqu'on n'a plus d'assignations contraintes, la propagation
	   s'arrête. On rentre la liste des assignations effectuée au cours de
	   cette propagation dans une liste, et on passe au prochain pari. *)
	else begin
	  let x = St.choose setv' in
	  if debug then begin
	    print_string "Choice: ";
	    print_int x;
	    print_newline()
	  end;
	  Cor.write x; Oper.propag x;
	  (* On assigne x à vrai, et on rentre cette assignation dans une liste,
	     afin de désassigner convenablement lors du potentiel backtrack. *)
	  let (_, m) = Elt.extract x env.clause in
	  let lc' = Elt.find (-x) env.clause in
	  aux {clause = m; order = Cor.tl env.order} lc' setv'
	end
      in aux env lc St.empty
      
    let bindings env = Elt.bindings env.clause

    let update env = {clause = env.clause; order = Cor.update env.order}

  end;;


module type OpAbstract = functor (Elt : OpElt) -> functor (Cor : CoreElt) ->

  sig
    exception Satisfiable
    exception Unsatisfiable
    type env
    type cls
    type map
    val create : unit -> env
    val propag : int -> unit
    val flush : unit -> unit
    val restore : unit -> unit
    val reset : unit -> unit
    val split : env -> (int * (cls list * env) * (cls list * env))
    val is_empty : env -> bool
    val propagation : env -> cls list -> env
    val bindings : env -> (int * int list list) list
    val update : env -> env
  end;;

module Make = (OpCore : OpAbstract);;
