(* Regroupe les modules d'initialisation *)

module Core =
  struct
    
    let (var, cls) = Scanf.bscanf (Scanf.Scanning.open_in "test") "p cnf %d %d" (fun v c -> (v, c) );;
    
    type order = (int * int) list
    
    let assigArray = Array.create var 0
    let read n = assigArray.(n - 1)
    let write n x = assigArray.(n - 1) <- x
    
    let hd l = snd (List.hd l)
    let tl = List.tl
    let init l = List.map (fun x -> x, x) l
    
    let fold = List.fold_right
    
  end;;

module type Abstract =
  sig
    type order
    val var : int
    val cls : int
    val read : int -> int
    val write : int -> int -> unit
    val hd : order -> int
    val tl : order -> order
    val init : int list -> order
    val fold : (int -> 'b -> 'b) -> int list -> 'b -> 'b
  end;;

module Make = (Core : Abstract);;


(* Tests *)


Make.var;;
Make.cls;;
Make.read 1;;
Make.write 3 2;;
Make.read 3;;




(* Anciennes versions *)


(* Assig *)
(* Gere l'assignation des variables *)

(*
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


module type OrderAbstract =
  sig
    type order
    val hd : order -> int

    val tl : order -> order
    val init : int list -> order
  end;;


module Order = (OrderCore: OrderAbstract);;
*)



(*
module Core = Make
  (struct
    let var = 10
    let cls = 10
    let lst = []
  end);;
*)
