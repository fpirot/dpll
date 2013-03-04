module type A = 
  sig
    type t
    val empty : t
  end;;
(* module de base servant à remplir un tableau dynamique. *)

module DynamicArray = functor (A : A) ->
  struct
    type t = {mutable tab : A.t array; mutable length : int}
    type a = A.t 

    let empty = {tab = [||]; length = 0};;
    let vect = empty
    let length = vect.length
    let read i = if i < length then vect.tab.(i) else failwith "Failure : out of bounds"
    let grow t = let tab = Array.make (2 * t.length + 1) A.empty in
		 for i = 0 to (t.length - 1) do tab.(i) <- t.tab.(i) done;
		 t.tab <- tab
    let reduce t = let tab = Array.make (t.length) A.empty in
		   for i = 0 to (t.length -1) do tab.(i) <- t.tab.(i) done;
		   t.tab <- tab
    let add x = let n = Array.length vect.tab in
		  if n = length then grow vect; 
      vect.tab.(length) <- x; 
      vect.length <- vect.length + 1
    let rm i = if i < length then vect.tab.(i) <- vect.tab.(length - 1);
      vect.length <- vect.length - 1;
      if Array.length vect.tab > 4 * length then reduce vect
  end ;;
(* Structure de tableau dynamique *)

module ClauseRef = functor (Clause : A) ->
  DynamicArray (Clause)
