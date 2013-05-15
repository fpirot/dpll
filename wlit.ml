module type Core =
sig
  type cls = int
  exception Satisfiable
  exception Unsatisfiable of cls
  val var : int
  val nb_cls : unit -> int
  val read : int -> int
  val write_father : int -> cls -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val literals : cls -> int list
end;;

module WlitCore =  functor (Cor: Core) -> 
struct

  type cls = Cor.cls

  module St = Set.Make (
    struct
      type t = int
      let compare = compare
    end)

  type set = St.t

  module Watched = Map.Make (struct
    type t = cls
    let compare = compare
  end)

  type watched = (int * int) Watched.t
  (* Associe à chaque clause son couple de variables surveillées. *)

  module Assoc = Map.Make (struct
    type t = int
    let compare x y = compare (abs x) (abs y)
  end)

  type assoc = St.t Assoc.t
(* Associe à chaque littéral l'ensemble des clauses dans lesquelles il
   est surveillé. *)

  type wlit = watched * assoc

  let wempty = (Watched.empty, Assoc.empty)

  let watched c w = Watched.find c (fst w)

  let assoc x w = Assoc.find x (snd w)

  let get_sat x c w = let (a,b) = watched c w in
		      x = a || x = b

  exception Finished of int * int
  let make_watched c sbord ssat =
    try (
      match List.fold_right (fun x (a,b) -> 
	match (Cor.read x) with
	  | y when y = x -> raise Cor.Satisfiable
	  | 0 -> if a = 0 then (a,0) else raise (Finished (a,x))
	  | _ -> (a,b)) (Cor.literals c) (0,0)
      with
	| (0,0) -> raise (Cor.Unsatisfiable c)
	| (a,0) -> Cor.write_father a c; ((a,0), St.add a sbord, ssat)
	| _ -> failwith "Match error: wlit")
    with
      | Cor.Satisfiable -> ((0,0), sbord, St.add c ssat)
      | Finished (a,b) -> ((a,b), sbord, ssat)

  let add_cls c wlit sbord ssat = 
    let ((a,b), sbord', ssat') = make_watched c sbord ssat in
    let watched = Watched.add c (a,b) (fst wlit) in
    let assoc1 = if a <> 0 then Assoc.add a (St.add c (try Assoc.find a (snd wlit) with Not_found -> St.empty)) (snd wlit) else snd wlit in
    let assoc2 = if b <> 0 then Assoc.add b (St.add c (try Assoc.find b assoc1 with Not_found -> St.empty)) assoc1 else assoc1 in
    ((watched, assoc2), sbord', ssat')

  let update_cls n wlit =
    let p = Cor.nb_cls () in
    let rec aux n =
      if n = p then wlit
      else (fun (x,y,z) -> x) (add_cls n (aux (n+1)) St.empty St.empty) 
    in aux n

  let update x w =
    St.fold (fun c (w, sbord, ssat) -> if get_sat x c w then (w, sbord, St.add c ssat) else add_cls c w sbord ssat) (assoc x w) (w, St.empty, St.empty)
    
  let fold = St.fold
  let choose = St.choose
  let singleton = St.singleton
  let empty = St.empty
  let is_empty = St.is_empty
  let union = St.union
  let add = St.add
  let remove = St.remove
  let elements = St.elements
end;;

module type WlitAbstract = functor (Cor: Core) -> 
sig
  type set
  type wlit
  type cls = Cor.cls
  val wempty : wlit
  val update : int -> wlit -> wlit * set * set
  val update_cls : cls -> wlit ->  wlit
  val fold : (cls -> 'a -> 'a) -> set -> 'a -> 'a
  val choose : set -> int
  val singleton : int -> set
  val empty : set
  val is_empty : set -> bool
  val union : set -> set -> set
  val add : int -> set -> set
  val remove : int -> set -> set
  val elements : set -> int list
end;;

module Make = (WlitCore: WlitAbstract);;
