(* Assig *)
(* Gere l'assignation des variables *)

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

module type OrderAbstract =    sig
    type order
    val hd : order -> int
    val tl : order -> order
    val init : int list -> order
  end;;

module Order = (OrderCore: OrderAbstract);;


(* Tests *)

(*
module TestAssig = Assig
  (struct
    type t = int
    let zero = 0
    let nbr = 10
  end);;
*)
