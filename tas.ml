module type A = 
sig
  type t
  val compare : t -> t -> int
  val print : t -> unit
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
	if y << (x,0) then N (incr_size y, add x fg, fd)
	else N ((x, snd y + 1), add (fst y) fg, fd)
      else 
	if y << (x,0) then N (incr_size y, fg, add x fd)
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
	      |false, false -> N ((fst a2, snd a2 + snd a1), t1, aux (fg2, fd2))
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

  let rec print = function
    |Nil -> ()
    |N(x, fg, fd) -> A.print (fst x);
      if fg <> Nil then (print_string " ( "; print fg; print_string " ) ");
      if fd <> Nil then (print_string " ( "; print fd; print_string " ) ")

  let rec iter f = function
    |Nil -> ()
    |N(x, fg, fd) -> f (fst x); iter f fg; iter f fd

  let rec fold f t a = match t with
    |Nil -> a
    |N (x, fg, fd) -> fold f fd (fold f fg (f (fst x) a))

  (* Construit un tas à partir d'une liste l, en temps linéaire. *)
  let build l =
    let last_line_length n = 
      let rec aux = function
	|0 -> 0
	|1 -> 0
	|n -> n mod 2 + 2 * (aux (n/2))
      in aux n + 1 in
    let n = List.length l in
    let p = last_line_length n in
    (* Crée la dernière ligne de singletons, en puisant les éléments
       dans une liste, et la remplit avec des arbres vides si
       besoin. Renvoie aussi la listes des éléments restants. *)
    let fill_last_line l =
      (* Remplit la fin de la ligne avec des arbre vides, pour avoir une
	 dernière ligne d'une taille 2^q pour un certain q. *)
      let rec fill_Nil = function
	|0 -> []
	|n -> Nil :: fill_Nil (n - 1) in
      let rec aux lbuilt l = function
	|0 -> (lbuilt, l)
	|n -> aux (singleton (List.hd l) :: lbuilt) (List.tl l) (n-1)
      in aux (fill_Nil (n - p + 1)) l p in
    (* Fusionne les tas t1 et t2 à l'aide de l'élément x, inséré
       convenablement. *)
    let rec fusion x t1 t2 = match t1, t2 with
      |(Nil, Nil) -> singleton x
      (* Dans les deux cas qui suivent, t1/t2 est forcément un
	 singleton vu que l'on travaille sur des tas équilibrés. *)
      |(_, Nil) -> let a = min_elt t1 in
		   if (x,0) <<= (a,0) then N ((x,2), t1, Nil)
		   else N ((a,2), singleton x, Nil)
      |(Nil, _) -> let a = min_elt t2 in
		   if (x,0) <<= (a,0) then N ((x,2), Nil, t2)
		   else N ((a,2), Nil, singleton x)
      |(N (a1, fg1, fd1), N (a2, fg2, fd2)) ->
	if (x,0) <<= a1 && (x,0) <<= a2 then N ((x, snd a1 + snd a2 + 1), t1, t2)
	else if a1 <<= (x,0) && a1 <<= a2 then N ((fst a1, snd a1 + snd a2 + 1), fusion x fg1 fd1, t2)
	else N ((fst a2, snd a1 + snd a2 + 1), t1, fusion x fg2 fd2) in
    (* Effectue les fusions le long d'une ligne du tas. *)
    let rec line_fusion lbuilt lref = function
      |0 -> [], lref
      |n -> match lbuilt, lref with
	  |(t1 :: t2 :: rbuilt, x :: rref) -> let (l1, l2) = line_fusion rbuilt rref (n-1) in (fusion x t1 t2 :: l1, l2)
	  |_ -> failwith "Erreur dans les arguments" in
    let rec aux lbuilt lref = function
      |0 -> [Nil]
      |1 -> fst (line_fusion lbuilt lref 1)
      |n -> let (lbuilt', lref') = line_fusion lbuilt lref n in aux lbuilt' lref' (n / 2) in
    let (lbuilt, lref) = fill_last_line l in List.hd (aux lbuilt lref ((n - p + 1) / 2))
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
  val print : t -> unit
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val build : elt list -> t
end;;

module Make = (TasCore : TasAbstract);;

(* Tests *)
(*
module Tas = Make (struct
  type t = int
  let compare = compare
  let print = print_int
end);;

let t = ref Tas.empty;;

for i = 1 to 10 do
  t := Tas.add i !t
done;;

Tas.size !t;;
Tas.print !t;;

let _,tas =  Tas.extract_min !t in t := tas;
Tas.print !t;;

let t = Tas.built [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];;

let constr = 
  let rec aux l = function
  |0 -> l
  |n -> aux (n :: l) (n-1)
  in aux [];;

let l = constr 200000;;

let t = Tas.build l;;

Tas.size t;;
*)
