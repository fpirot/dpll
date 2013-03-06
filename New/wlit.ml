exception Unsatisfiable;;

module type Clause =
  sig
    val cls : int
    val literals : int -> int list
  end;;

module type Assig =
  sig
    val nbr : int
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

    let watched_to_clauses x = assoc.(x-1)
(* Renvoie la liste des indices de clauses dans lesquelles la variable x est surveillé. *)

    let read id = warray.(id)
(* Renvoie les litéraux surveillés pour la clause d'indice id. *)

    let get_sat x id = let (a, b) = read id in
		       x = a || x = b
(* Détermine si l'assignation du litéral x à vrai rend la clause d'indice id satisfiable, au vu des litéraux qu'elle surveille. *)

    let fill_assoc () = 
      let n = Elt.cls in
      for i = 0 to n-1 do
	let (a,b) = warray.(i) in
	assoc.((abs a) - 1) <- i :: assoc.(a - 1);
	assoc.((abs b) - 1) <- i :: assoc.(b - 1)
      done
(* Remplit la table d'association entre watched literals et clauses. *)

    let watched_literals_of_clause id lbord lsat =
      let l = Elt.literals id in
      let rec aux w1 w2 = function
	|[] -> if w1 = 0 then raise Unsatisfiable
      (* Si on n'a trouvé aucun litéral à surveiller, la clause n'est pas satisfiable avec la valuation actuelle. *)
	  else (lbord := w1 :: !lbord; (w1,0))
    (* Si on n'a pu trouver qu'un seul litéral à surveiller, alors pour que la clause soit satisfaite, il doit forcément être à vrai. *)
	|x :: r -> let v = Assig.read (abs x) in
		   if v = 0 then 
		     if w1 = 0 then aux x 0 r
		     else (w1,x)
		   else if v = x then (lsat := id :: !lsat; (0,0))
		(* c est satisfiable, on la répertorie dans lsat. *)
		   else aux w1 w2 r
      in aux 0 0 l
(* watched_literals_of_clause renvoie un couple de litéraux à surveiller possibles pour la clause c, et remplit la liste lbord avec x si x est le seul litéral pouvant encore être potentiellement vrai dans c, et lsat avec c si elle est satisfiable. *)

    let fill_warray () =
      let n = Elt.cls in
      let lbord = ref [] and lsat = ref [] in
      for i = 0 to n-1 do
	warray.(i) <- watched_literals_of_clause i lbord lsat
      done;
      (!lbord, !lsat)
(* A partir d'un tableau de clauses, et d'une fonction Assig.read qui donne la valuation des variables en cours,
remplit warray, et renvoie la liste des assignations nécessaires (pour les clauses singleton) et la liste des clauses satisfiables. *)

    let update x = 
      let l = watched_to_clauses (abs x) in
      let lbord = ref [] and lsat = ref [] in
      let rec aux = function
	|[] -> (!lbord, !lsat)
	|i :: r -> if get_sat x i then aux  r
	  else (let (a, b) = watched_literals_of_clause i lbord lsat in warray.(i) <- (a, b);
		assoc.((abs a) - 1 ) <- i :: assoc.((abs a) - 1);
		assoc.((abs b) - 1 ) <- i :: assoc.((abs b) - 1);
		aux r)
      in aux l
(* Change les litéraux à surveiller dans warray, lorsqu'une nouvelle variable voit sa valeur fixée. Renvoie la liste des litéraux à assigner à vrai par effet de bord, et la liste des clauses nouvellement satisfiables. *)

  end;;
