exception Unsatisfiable;;

module type Clause =
sig
  val literals :int -> int list
end;;

module type Assig =
sig
  val cls : int
  val var : int
  val read : int -> int
end;;

module WlitCore = functor (Elt: Clause) -> functor (Assig: Assig) -> 
struct

  module St = Set.Make (
    struct
      type t = int
      let compare = compare
    end)

  type set = St.t

  type wlit = int * int
  let zero = (0, 0)
  let warray = Array.make Assig.cls zero

  let assoc = Array.make Assig.var []
  (* table d'association : à la variable i est associée l'ensemble
     des clauses (représentées par leur indice dans un tableau les
     listant toutes) dans lesquelle elle est surveillée. *)

  let watched_to_clauses x = assoc.(x-1)
  (* Renvoie la liste des indices de clauses dans lesquelles la
     variable x est surveillé. *)

  let read id = warray.(id)
  (* Renvoie les litéraux surveillés pour la clause d'indice id. *)

  let get_sat x id = let (a, b) = read id in
		     x = a || x = b
  (* Détermine si l'assignation du litéral x à vrai rend la clause
     d'indice id satisfiable, au vu des litéraux qu'elle surveille. *)

  let fill_assoc () = 
    let n = Assig.cls in
    for i = 0 to n-1 do
      let (a,b) = warray.(i) in
      assoc.((abs a) - 1) <- i :: assoc.(a - 1);
      assoc.((abs b) - 1) <- i :: assoc.(b - 1)
    done
  (* Remplit la table d'association entre watched literals et
     clauses. *)

  let watched_literals_of_clause id lbord lsat =
    let l = Elt.literals id in
    let rec aux w1 w2 = function
      |[] -> if w1 = 0 then raise Unsatisfiable
	(* Si on n'a trouvé aucun litéral à surveiller, la clause
	   n'est pas satisfiable avec la valuation actuelle. *)
	else (lbord := St.add w1 !lbord; (w1,0))
      (* Si on n'a pu trouver qu'un seul litéral à surveiller, alors
	 pour que la clause soit satisfaite, il doit forcément être à
	 vrai. *)
      |x :: r -> let v = Assig.read (abs x) in
		 if v = 0 then 
		   if w1 = 0 then aux x 0 r
		   else (w1,x)
		 else if v = x then (lsat := St.add id !lsat; (0,0))
		 (* c est satisfiable, on la répertorie dans lsat. *)
		 else aux w1 w2 r
    in aux 0 0 l
  (* watched_literals_of_clause renvoie un couple de litéraux à
     surveiller possibles pour la clause c, et remplit la liste lbord
     avec x si x est le seul litéral pouvant encore être
     potentiellement vrai dans c, et lsat avec c si elle est
     satisfiable. *)

  let new_assoc id lbord lsat =
    let (a, b) = watched_literals_of_clause id lbord lsat in
    if a <> 0 then assoc.((abs a) - 1 ) <- id :: assoc.((abs a) - 1);
    if b <> 0 then assoc.((abs b) - 1 ) <- id :: assoc.((abs b) - 1);
    warray.(id) <- (a, b)
  (* Associe de nouveaux watched literals à la clause d'indice i, et
     modifie les tables convenablement. *)

  let fill_warray () =
    let n = Assig.cls in
    let lbord = ref St.empty and lsat = ref St.empty in
    for i = 0 to n-1 do
      warray.(i) <- watched_literals_of_clause i lbord lsat
    done
  (* A partir d'un tableau de clauses remplit warray. *)

  let init () =
    let _ = fill_warray () in
    fill_assoc ()

  let update x = 
    let l = watched_to_clauses (abs x) in
    let lbord = ref St.empty and lsat = ref St.empty in
    let rec aux = function
      |[] -> (!lbord, !lsat)
      |i :: r -> if get_sat x i then (lsat := St.add i !lsat; aux r)
	else (new_assoc i lbord lsat;	aux r)
    in aux l
(* Change les litéraux à surveiller dans warray, lorsqu'une nouvelle
   variable voit sa valeur fixée. Renvoie la liste des litéraux à
   assigner à vrai par effet de bord, et la liste des clauses
   nouvellement satisfiables. *)

  let fold = St.fold
  let choose = St.choose
  let singleton = St.singleton
  let empty = St.empty
  let is_empty = St.is_empty
  let union = St.union
  let add = St.add
  let remove = St.remove
end;;

module type WlitAbstract = functor (Elt: Clause) -> functor (Assig: Assig) -> 
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

module Make = (WlitCore: WlitAbstract);;
