module type Core =
sig
  exception Satisfiable
  exception Unsatisfiable
  type cls = int
  val var : int
  val cls : int
  val read : int -> int
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val literals : cls -> int list
end;;

module WlitCore =  functor (Cor: Core) -> 
struct

  type cls = Cor.cls

  module St = Set.Make (
    struct
      type t = int * cls
      let compare x y = compare (fst x) (fst y)
    end)

  module Stc = Set.Make (
    struct
      type t = cls
      let compare = compare
    end)

  type set = St.t
  type setc = Stc.t

  type wlit = int * int
  let zero = (0, 0)
  let warray = Array.make Cor.cls zero

  let assoc = Array.make Cor.var []
  (* table d'association : à la variable i est associée l'ensemble
     des clauses (représentées par leur indice dans un tableau les
     listant toutes) dans lesquelle elle est surveillée. *)

  let print_warray () = 
    print_string "Watched literals :";
    Array.iter (fun x -> print_char '('; print_int (fst x); print_char ' '; print_int (snd x); print_string ") ") warray;
    print_newline()

  let watched_to_clauses x = if x <> 0 then assoc.((abs x) - 1) else []
  (* Renvoie la liste des indices de clauses dans lesquelles la
     variable x est surveillé. *)

  let read id = warray.(id)
  (* Renvoie les litéraux surveillés pour la clause d'indice id. *)

  let get_sat x id = let (a, b) = read id in
		     x = a || x = b
  (* Détermine si l'assignation du litéral x à vrai rend la clause
     d'indice id satisfiable, au vu des litéraux qu'elle surveille. *)

  let fill_assoc () = 
    let n = Cor.cls in
    for i = 0 to n-1 do
      let (a,b) = warray.(i) in
      if a <> 0 then  assoc.((abs a) - 1) <- i :: assoc.((abs a) - 1);
      if b <> 0 then assoc.((abs b) - 1) <- i :: assoc.((abs b) - 1)
    done
  (* Remplit la table d'association entre watched literals et
     clauses. *)

  let watched_literals_of_clause id lbord lsat =
    let l = Cor.literals id in
    let rec aux w1 w2 = function
      |[] -> if w1 = 0 then raise Cor.Unsatisfiable
	(* Si on n'a trouvé aucun litéral à surveiller, la clause
	   n'est pas satisfiable avec la valuation actuelle. *)
	else (lbord := St.add (w1, id) !lbord; (w1,0))
      (* Si on n'a pu trouver qu'un seul litéral à surveiller, alors
	 pour que la clause soit satisfaite, il doit forcément être à
	 vrai. On effectue l'assignation nécessaire. *)
      |x :: r -> let v = Cor.read x in
		 if v = 0 then 
		   if w1 = 0 then aux x 0 r
		   else (w1,x)
		 else if v = x then (lsat := Stc.add id !lsat; (0,0))
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
    let n = Cor.cls in
    let lbord = ref St.empty and lsat = ref Stc.empty in
    for i = 0 to n-1 do
      warray.(i) <- watched_literals_of_clause i lbord lsat
    done
  (* A partir d'un tableau de clauses remplit warray. *)

  let init () =
    let _ = fill_warray () in
    fill_assoc ()

  let update x =
    let l = watched_to_clauses x in
    let lbord = ref St.empty and lsat = ref Stc.empty in
    let rec aux = function
      |[] -> (!lbord, !lsat)
      |i :: r -> if get_sat x i then (lsat := Stc.add i !lsat; aux r)
	else (new_assoc i lbord lsat; aux r)
    in aux l
(* Change les litéraux à surveiller dans warray, lorsqu'une nouvelle
   variable voit sa valeur fixée. Renvoie la liste des litéraux à
   assigner à vrai par effet de bord, et la liste des clauses
   nouvellement satisfiables. *)

  let fold = Stc.fold
  let choose = St.choose
  let singleton x = St.singleton (x, -1)
  let empty = St.empty
  let is_empty = St.is_empty
  let union = St.union
  let add = St.add
  let remove x = St.remove (x, 0)
  let elements = St.elements
end;;

module type WlitAbstract = functor (Cor: Core) -> 
sig
  type set
  type setc
  type cls = Cor.cls
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

module Make = (WlitCore: WlitAbstract);;
