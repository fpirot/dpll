open Dpll;;

exception Unsatisfiable;;

exception Val of int;;
(* Quand une variable est contrainte à une valeur, on renvoie l'exception Val avec en argument le litéral forcé. *)

exception Satisfiable of int;;
(* Quand une clause est satisfiable, l'exception a en argument l'indice de la clause. *)

module type Clause =
  sig
    type t
    val empty : t
    val var : t -> int list
  end;;


module Wlit = functor (Clause : Clause) ->
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

    let watched_literals_of_clause c =
  let l = Clause.var c in
  let rec aux w1 w2 = function
    |[] -> if w1 = 0 then raise Unsatisfiable
(* Si on n'a trouvé aucun litéral à surveiller, la clause n'est pas satisfiable avec la valuation actuelle. *)
      else raise Val w1
(* Si on n'a pu trouver qu'un seul litéral à surveiller, alors pour que la clause soit satisfaite, il doit forcément être à vrai. *)
    |x :: r -> let v = Assig.read (abs x) in
      if v = 0 then 
	if w1 = 0 then aux x 0 r
	else (w1,x)
      else if v = x then raise Satisfiable i
      else aux w1 w2 r
  in aux 0 0 l
(* watched_literals_of_clause renvoie un couple de litéraux à surveiller possibles pour la clause c. *)

    let clauses_to_wclauses tab =
  let n = Array.length tab in
  let t = Array.make n {lit = Clause.empty; wlit = (0,0)} in
for i = 0 to n-1 do
  let c = tab.(i) in
  t.(i).lit <- c;
  let d = watched_literals_of_clause c in
  t.(i).wlit <- d
done;
t
(* A partir d'un tableau de clauses, et d'une fonction Assig.read qui donne la valuation des variables en cours,
construit un tableau de clauses avec deux litéraux surveillés. *)  

    let update_watched_literals x t assoc = 
  let l = watched_to_clauses (abs x) assoc in
  let rec aux = function
    |[] -> ()
    |i :: r -> let d = watched_literals_of_clause t.(i) in t.(i).wlit <- d; aux r
in aux l
(* Change les litéraux à surveiller dans le tableau t, lorsqu'une nouvelle variable voit sa valeur fixée. *)

  end;;
