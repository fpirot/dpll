module type A = 
  sig
    type t
    val empty : t
  end;;
(* module de base servant à remplir un tableau dynamique. *)

module DaCore = functor (A: A) ->
struct
  type t = {mutable tab: A.t array; mutable length: int}
  type elt = A.t 

  let empty = {tab = [||]; length = 0};;
  let length t = t.length
  let is_empty t = length t = 0
  let read i t = if i < length t then t.tab.(i) else failwith "Failure: out of bounds"
  let grow t = let tab = Array.make (2 * t.length + 1) A.empty in
	       Array.blit t.tab 0 tab 0 t.length;
	       t.tab <- tab
  let reduce t = let tab = Array.make (t.length) A.empty in
		 Array.blit t.tab 0 tab 0 t.length;
		 t.tab <- tab
  let add x t = let n = Array.length t.tab in
		if n <= length t then grow t; 
		t.tab.(length t) <- x;
		t.length <- t.length + 1
  let rm i t = if i < length t then t.tab.(i) <- t.tab.(length t - 1);
    t.length <- t.length - 1;
    if Array.length t.tab > 4 * length t then reduce t

  let iter f t = for i = 0 to length t do f (read i t) done

  let write x i t = if (i < 0 || i >= length t) then failwith "Failure: out of bounds"
	else t.tab.(i) <- x
end ;;
(* Structure de tableau dynamique *)

module type DaAbstract = functor (A: A) ->
sig
  type t
  type elt = A.t
  val empty : t
  val is_empty : t -> bool
  val length : t -> int
  val read : int -> t -> elt
  val add : elt -> t -> unit
  val iter : (elt -> unit) -> t -> unit
  val write : elt -> int -> t -> unit
end;;

module Make = (DaCore: DaAbstract)
