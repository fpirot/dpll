exception Unsatisfiable;;

exception Val of int;;
(* Quand une variable est contrainte à une valeur, on renvoie l'exception Val avec en argument le litéral forcé. *)

exception Satisfiable of int;;
(* Quand une clause est satisfiable, l'exception a en argument l'indice de la clause. *)

module Literal = 
  struct
    type t = int
    let compare x y = compare (abs x) (abs y)
(* On compare selon le nom de la variable, indépendemment de la valeur du litéral correspondant. *)
  end;;
(* Structure des litéraux qui composent les clauses. *)

module type Clause = 
  sig
    type t = Set.Make(Literal).t
    val for_all : (int -> bool) -> t -> bool
    val iter : (int -> unit) -> t -> unit
    val mem : int -> t -> bool
    val remove : int -> t -> t
    val elements : t -> int list
    val empty : t
  end;;

module type AssigElt =
  sig
    type t
    val zero : t
    val nbr : int
  end;;
(* nbr est le nombre de variables dans l'instance de SAT, indiquée en début de problème. *)

module AssigCore = functor (Elt : AssigElt) ->
  struct
    type t = Elt.t
    let assigArray = Array.create Elt.nbr Elt.zero
    let read n = assigArray.(n - 1)
    let write n x = assigArray.(n - 1) <- x
  end;;

module type AssigAbstract = functor (Elt : AssigElt) ->
  sig
    type t = Elt.t
    val read : int -> t
    val write : int -> t -> unit
  end;;

module Assig = (AssigCore : AssigAbstract);;

module Wlit = functor (Clause: Clause) -> functor (Assig: Assig) ->
  struct

    type  wclause = {mutable lit : Clause.t; mutable wlit : int * int}
(* lit contient l'ensemble des litéraux de la clause, wlit contient les deux variables surveillées dans la clause (dont la valuation est donc indéterminée). *)

    type assoc = int array
(* table d'association : à la variable i est associée l'ensemble des clauses
  (représentées par leur indice dans un tableau les listant toutes) dans lesquelle elle est surveillée. *)

    let watched_to_clauses x assoc = assoc.(x-1)

    let fill_assoc tab = 
      let n = Array.length tab in
      let t = Array.make n [] in
    for i = 0 to n-1 do
      let c = tab.(i) in
      let (a,b) = c.wlit in
      t.(a-1) <- c :: t.(a-1);
      t.(b-1) <- c :: t.(b-1)
    done;
    t
(* Remplit la table d'association entre watched literals et clauses, avec un tableau de wclauses en entrée. *)

    let watched_literals_of_clause t i =
  let l = Clause.elements t.(i).lit in
  let rec aux w1 w2 = function
    |[] -> if w1 = 0 then raise Unsatisfiable
(* Si on n'a trouvé aucun litéral à surveiller, la clause n'est pas satisfiable avec la valuation actuelle. *)
      else raise (Val w1)
(* Si on n'a pu trouver qu'un seul litéral à surveiller, alors pour que la clause soit satisfaite, il doit forcément être à vrai. *)
    |x :: r -> let v = Assig.read (abs x) in
      if v = 0 then 
	if w1 = 0 then aux x 0 r
	else (w1,x)
      else if v = x then raise (Satisfiable i)
      else aux w1 w2 r
  in aux 0 0 l
(* watched_literals_of_clause renvoie un couple de litéraux à surveiller possibles pour la clause c. *)

    let clauses_to_wclauses tab =
  let n = Array.length tab in
  let t = Array.make n {lit = Clause.empty; wlit = (0,0)} in
for i = 0 to n-1 do
  let c = tab.(i) in
  t.(i).lit <- c;
  let d = watched_literals_of_clause t i in
  t.(i).wlit <- d
done;
t
(* A partir d'un tableau de clauses, et d'une fonction Assig.read qui donne la valuation des variables en cours,
construit un tableau de clauses avec deux litéraux surveillés. *)  

    let update_watched_literals x t assoc = 
  let l = watched_to_clauses (abs x) assoc in
  let rec aux = function
    |[] -> ()
    |i :: r -> let d = watched_literals_of_clause t i in t.(i).wlit <- d; aux r
in aux l
(* Change les litéraux à surveiller dans le tableau t, lorsqu'une nouvelle variable voit sa valeur fixée. *)

  end;;



module Clause = Set.Make (Literal);;
let rec list_to_clause = function
  |[] -> Clause.empty
  |x :: r -> Clause.add x (list_to_clause r);;
(* Une clause est représenté par un ensemble de variables. *)

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
    val is_unsat : set -> bool
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



module ClauseCore = functor (Elt : ClauseElt) -> functor (Ord : OrdElt) ->
  struct
    type env = {clause: Elt.map; order: Ord.order}
    type set = Elt.set
    type map = Elt.map

(* Extrait une variable selon l'ordre *)
    let split env =
      let k = Ord.hd env.order in
      let (l, m) = Elt.extract k env.clause in
        ((k, l),
          {clause = m;
            order = Ord.tl env.order})
    
(* Determine l'assignation d'une variable *)    
    let assignment (x, lstC) =
    
      let rec assigC optT optF = function
        |[] -> optT, optF
        |c::l -> match optT, optF with
          | None, None -> failwith "Assignment"
          | Some lstT, None -> begin match assigV c with
              | true, _ -> assigC optT optF l
              | false, None -> raise Unsatisfiable
              | false, Some v -> assigC (Some (v :: lstT)) None l
            end
          | None, Some lstF -> begin match assigV c with
              | false, _ -> assigC optT optF l
              | true, None -> raise Unsatisfiable
              | true, Some v -> assigC None (Some (v :: lstF)) l
            end
          | Some lstT, Some lstF -> begin match assigV c with
              | false, None -> assigC None  (Some lstF) l
              | true, None -> assigC (Some lstT) None l
              | false, Some v -> assigC (Some (v :: lstT)) optF l
              | true, Some v -> assigC optT (Some (v :: lstF)) l
            end
      
      and assigV c =
        let (b, c) = Elt.variable x c in
          if Elt.is_unsat c then b, None else b, Some c         
      
    in assigC (Some []) (Some []) lstC
     
  end;;


module type ClauseAbstract = functor (Elt : ClauseElt) -> functor (Ord : OrdElt) ->
  sig
    type env
    type set
    type map
    val split : env -> (int * set list) * env
    val assignment : int * set list -> set list option * set list option
  end;;


module Op = (ClauseCore : ClauseAbstract);;




(* Order *)
(* Determine l'ordre dans lequel on considere les variables.
La fonction d'ordre en elle meme n'est pas encore implementee  *)

module OrderCore =
  struct
    type order = (int * int) list
    let hd l = snd (List.hd l)
    let tl = List.tl
    let init l = List.map (fun x -> x, x) l
  end;;

module type OrderAbstract =    sig
    type order
    val hd : order -> int
    val tl : order -> order
    val init : int list -> order
  end;;

module Order = (OrderCore: OrderAbstract);;


(* Tests *)

module TestAssig = Assig
  (struct
    type t = int
    let zero = 0
    let nbr = 10
  end);;

module TestClause = Clause;;

module Test = Wlit (TestClause) (TestAssig);;

