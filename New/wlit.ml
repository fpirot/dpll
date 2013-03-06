open Dpll;;

exception Unsatisfiable;;

exception Val of int;;
(* Quand une variable est contrainte à une valeur, on renvoie l'exception Val avec en argument le litéral forcé. *)

exception Satisfiable of int;;
(* Quand une clause est satisfiable, l'exception a en argument l'indice de la clause. *)

module type Clause =
  sig
    val cls : int
    val literals : int -> int list
  end;;

module type Assig =
  sig
    val read : int -> int
  end;;

module Wlit = functor (Elt: Clause) -> functor (Assig: Assig) ->
  struct
    type wlit = int * int
    let zero = (0, 0)
    let warray = Array.make Elt.cls zero

    let assoc = Array.make Assig.nbr []
(* table d'association : à la variable i est associée l'ensemble des clauses
  (représentées par leur indice dans un tableau les listant toutes) dans lesquelle elle est surveillée. *)

    let watched_to_clauses x assoc = assoc.(x-1)

    let fill_assoc () = 
      let n = Elt.cls in
      for i = 0 to n-1 do
	let (a,b) = warray.(i) in
	assoc.(a-1) <- c :: t.(a-1);
	assoc.(b-1) <- c :: t.(b-1)
      done
(* Remplit la table d'association entre watched literals et clauses. *)

    let watched_literals_of_clause id =
      let l = Elt.literals id in
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

    let fill_warray () =
      let n = Elt.cls in
      for i = 0 to n-1 do
	warray.(i) <- watched_literals_of_clause i
      done
(* A partir d'un tableau de clauses, et d'une fonction Assig.read qui donne la valuation des variables en cours,
construit un tableau de clauses avec deux litéraux surveillés. *)  

    let update x = 
      let l = watched_to_clauses (abs x) assoc in
      let rec aux lst = function
	|[] -> lst
	|i :: r -> try (let d = watched_literals_of_clause i in warray.(i) <- d; aux lst r) with Satisfiable(i) -> aux (i::lst) r
      in aux l l
(* Change les litéraux à surveiller dans warray, lorsqu'une nouvelle variable voit sa valeur fixée. Renvoie la liste des indices de clauses qui ont vues leurs watched literals modifiés. *)

  end;;
