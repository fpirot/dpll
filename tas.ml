module type A = 
sig
  type t
  val compare : t -> t -> int
end;;

module TasCore = functor (A: A) ->
struct
  type elt = A.t
  type constr = elt * int
  type t = Nil | N of constr * t * t

  let (<<) (a: constr) (b: constr) = A.compare (fst a) (fst b) = -1
  let (>>) (a: constr) (b: constr) = A.compare (fst a) (fst b) = 1
  let (==) (a: constr) (b: constr) = A.compare (fst a) (fst b) = 0
  let (<<=) (a: constr) (b: constr) = a << b || a == b
  let (>>=) (a: constr) (b: constr) = a >> b || a == b

  let size = function
    |Nil -> 0
    |N (x,_,_) -> snd x

  let incr_size (a: constr) = fst a, snd a + 1

  let min t1 t2 = if size t1 < size t2 then t1 else t2

  let empty = Nil

  let singleton x = N ((x,1), Nil, Nil)

  let is_empty = (=) Nil

  let min_elt = function
    |Nil -> raise Not_found
    |N (x,_,_) -> fst x

  let rec add x = function
    |Nil -> singleton x
    |N(y,fg,fd) ->
      if size fg < size fd then
	if fst y < x then N (incr_size y, add x fg, fd)
	else N ((x, snd y + 1), add (fst y) fg, fd)
      else 
	if fst y < x then N (incr_size y, fg, add x fd)
	else N ((x, snd y + 1), fg, add (fst y) fd)

  let extract_min =
    let rec aux = function
      |Nil, t -> t
      |t, Nil -> t
      |t1, t2 -> 
	match t1, t2 with 
	  |N (a1, fg1, fd1), N (a2, fg2, fd2) -> (
	  let p1, p2 = size t1, size t2 in
	  match (a1 <<= a2, p1 >= p2) with
	    |true, true -> N ((fst a1, snd a1 + snd a2), aux (fg1, fd1), t2)
	    |false, false -> N ((fst a2, snd a2 + snd a1), t1, aux (fg1, fg2))
	    |false, true -> N ((fst a2, snd a2 + snd a1), aux (fg1, fd1), N ((fst a1, snd a2), fg2, fd2))
	    |true, false -> N ((fst a1, snd a1 + snd a2), N ((fst a2, snd a1), fg1, fd1), aux (fg2, fd2)))
	  |_ -> failwith "Error"
    in function
      |Nil -> raise Not_found
      |N (x, fg, fd) -> (fst x, aux (fg, fd))

  let bindings t =
    let l = ref [] in
    let rec aux = function
      |Nil -> ()
      |N(x, fg, fd) -> l := fst x :: !l; aux fg; aux fd
    in aux t; !l
end;;

module type TasAbstract = functor (A: A) ->
sig
  type t
  type elt = A.t
  val size : t -> int
  val singleton : elt -> t
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val min_elt : t -> elt
  val extract_min : t -> elt * t
  val bindings : t -> elt list
end;;

module Make = (TasCore : TasAbstract);;

(* Tests *)
(*
module Tas = Make (struct
  type t = int
  let compare = compare
end);;

let t = ref Tas.empty;;

for i = 1 to 10 do
  t := Tas.add i !t
done;;

Tas.min_elt !t;;

Tas.size !t;;

let _,tas =  Tas.extract_min !t in t := tas;;

Tas.size !t;;

Tas.min_elt !t;;
*)

(* Attention, la fonction extract_min fait perdre des éléments de
   temps en temps... *)
