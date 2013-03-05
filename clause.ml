exception Satisfiable;;
exception Unsatisfiable;;

(* Clause *)
(* Operation sur les clauses *)

module Literal = 
  struct
    type t = int
    let compare x y = compare (abs x) (abs y)
(* On compare selon le nom de la variable, indépendemment de la valeur du litéral correspondant. *)
  end;;
(* Structure des litéraux qui composent les clauses. *)

module Clause = Set.Make (Literal);;
let rec list_to_clause = function
  |[] -> Clause.empty
  |x :: r -> Clause.add x (list_to_clause r);;
(* Une clause est représenté par un ensemble de variables. *)

module type Clause = 
  sig
    type t = Set.Make(Literal).t
    val for_all : (int -> bool) -> t -> bool
    val iter : (int -> unit) -> t -> unit
    val mem : int -> t -> bool
    val remove : int -> t -> t
    val choose : t -> int
  end;;

module type Ref =
  sig
    type t
    val add : Clause.t -> unit
    val length : int
    val elt : int -> Clause.t
  end;;

module Int =
  struct
    type t = int
    let compare = compare
  end;;

module Element = Map.Make (Int);;
(* On classe les clauses dans une table d'association qui associe à chaque variable l'ensemble des clauses dans lesquelles elle apparaît. *)

module type Element = 
  sig
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val find : int -> 'a t -> 'a
    val remove : int -> 'a t -> 'a t
    val add : int -> 'a -> 'a t -> 'a t
  end;;

module Links = Set.Make (Int);;
(* L'ensemble servant à répertorier les clauses associées à une variable. *)

module type Links =
  sig
    type t
    val is_empty : t -> bool
    val add : int -> t -> t
    val singleton : int -> t
    val elements : t -> int list
    val remove : int -> t -> t
  end;;

module type Assig =
  sig
    type t
    val read : int -> int
    val write : int -> int -> unit
  end;;

module ClauseElt = functor (Elt : Element) -> functor (Links : Links) -> 
  functor (Ref : Ref) -> functor (Clause : Clause) -> functor (Assig : Assig) ->
  struct
    type set = Clause.t
(* Ref est un tableau dynamique de clauses, qui sert de référence à l'ensemble des problèmes les utilisant. *)

    type map = Links.t Elt.t

    let empty = Elt.empty

    let is_empty t = Elt.is_empty t

    let is_unsat s = Clause.for_all (fun x -> x + Assig.read x = 0) s

    let mem x t = not (Links.is_empty (Elt.find x t))

    let add s t = 
      Ref.add s;
      let n = Ref.length in
      let rep = ref t in
      Clause.iter (fun k ->
	try (let e = Elt.find (abs k) !rep in let e' = Links.add n e in rep := Elt.add (abs k) e' !rep);
	with Not_found -> rep := Elt.add (abs k) (Links.singleton n) !rep) s;
      !rep
      
    let variable x s = if Clause.mem x s then (true, Clause.remove x s)
      else if Clause.mem (-x) s then (false, Clause.remove (-x) s)
      else failwith "No such variable to remove."

    let extract x t = 
      let links = Elt.find x t in
      let l = Links.elements links in
      let rep = ref t in
      let rec aux = function
	|[] -> ()
	|k :: r -> let s = Ref.elt k in
	  Clause.iter (fun x -> let e = Elt.find (abs x) !rep in let e' = Links.remove k e in
	    rep := Elt.remove (abs x) !rep;
	    if not (Links.is_empty e') then rep := Elt.add (abs x) e' !rep) s;
          aux r
      in aux l;
      (List.map Ref.elt l, !rep)
  end;;


module type ClauseElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
  sig
    type set
    type map
    val empty : map
    val is_empty : map -> bool
    val are_sat : set -> int
    val find : int -> map -> set list
    val choose : set -> int
    val mem : int -> map -> bool (* Verifie si une variable est presente dans la map *)
    val add : set -> map -> map (* Ajoute une clause a la map *)
    val variable : int -> set -> bool * set (* Supprime une variable d'une clause et donne le booleen associe a son litteral *)
    val bindings : map -> (int * int list list) list (* Une fonction d'affichage *)
    val extract : int -> map -> set list * map (* Extrait la liste des clauses comportant une variable et renvoie la map privee de ces clauses *)
  end ;;

module type OrdElt =
  sig
    type order
    val hd : order -> int
    val tl : order -> order
  end;;

module ClauseCore = functor (Elt : ClauseElt) -> functor (Ord : OrdElt) -> functor (Assig : Assig) ->
  struct
    type env = {clause: Elt.map; order: Ord.order}
    type set = Elt.set
    type map = Elt.map

    module St = Set.Make (
      struct
	type t = int
	let compare = compare
      end)

(* Extrait une variable selon l'ordre *)
    let split env =
      let k = Ord.hd env.order in
      let (ltrue, mtrue) = Elt.extract k env.clause
      and (lfalse, mfalse) = Elt.extract (-k) env.clause in
      (k, (ltrue, {clause = mtrue; order = Ord.tl env.order}),
	  (lfalse, {clause = mfalse; order = Ord.tl env.order}))
    
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
	  Assig.write (abs x) x;
	  let (_, m) = Elt.extract x env.clause in
	  let lc' = Elt.find (-x) env.clause in
	  aux {clause = m; order = Ord.tl env.order} lc' setv'
	end
      in aux env lc St.empty

  end;;


module type ClauseAbstract = functor (Elt : ClauseElt) -> functor (Ord : OrdElt) -> functor (Assig : Assig) ->
  sig
    type env
    type set
    type map
    val split : env -> (int * (set list * env) * (set list * env))
    val is_empty : env -> bool
    val propagation : env -> set list -> env
  end;;


module Op = (ClauseCore : ClauseAbstract);;
