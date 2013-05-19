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

module St = Set.Make (
    struct
      type t = int * int
      let compare x y = compare (fst x) (fst y)
    end);;

module WlitCore =  functor (Cor: Core) -> 
struct

  type cls = Cor.cls

  module Stc = Set.Make (
    struct
      type t = cls
      let compare = compare
    end)

  type set = St.t
  type setc = Stc.t

  module Watched = Map.Make (struct
    type t = cls
    let compare = compare
  end)

  type watched = (int * int) Watched.t
  (* Associe à chaque clause son couple de variables surveillées. *)

  module Assoc = Map.Make (struct
    type t = int
    let compare = compare
  end)

  type assoc = Stc.t Assoc.t
  (* Associe à chaque littéral l'ensemble des clauses dans lesquelles il
     est surveillé. *)

  type wlit = watched * assoc

  let wempty = (Watched.empty, Assoc.empty)

  let watched c w = try Watched.find c (fst w) with Not_found -> failwith "plouf"

  let assoc x w = try Assoc.find x (snd w) with Not_found -> Stc.empty

  let get_sat x c w = let (a,b) = watched c w in
		      x = a || x = b

  let make_watched c =
    let rec aux a b = function
      | [] -> (match (a,b) with
	  | (0,0) -> raise (Cor.Unsatisfiable c)
	  | (a,0) -> (a,0)
	  | _ -> failwith "Match error: wlit")
      | x :: l -> (match Cor.read x with
	  | 0 -> if a = 0 then aux x 0 l else (a, x)
	  | _ -> aux a b l)
    in aux 0 0 (Cor.literals c);;

  let add_cls c wlit = 
    let (a,b) = make_watched c in
    (Watched.add c (a,b) (fst wlit), Assoc.add b (Stc.add c (assoc b wlit)) (Assoc.add a (Stc.add c (assoc a wlit)) (snd wlit)))

  let extract x (ltrue, lfalse) w =
    (* On retire toutes les clauses qui deviennent satisfaites *)
    let w1 = (fst w, (List.fold_right (fun c map -> 
      let (a,b) = watched c w in
      (* On considère le literal surveillé qui n'est pas x *)
      let y = if a = x then b else a in
      let s = Stc.remove c (assoc y w) in
      if s <> Stc.empty then Assoc.add y s map else map) ltrue (Assoc.remove x (snd w)))) in
    (* On attribue de nouveaux watched literals dans les clauses qui surveillaient -x *)
    List.fold_right add_cls lfalse w1 

  let update_cls n wlit =
    let p = Cor.nb_cls () in
    let rec aux n =
      if n = p then wlit
      else add_cls n (aux (n+1))
    in aux n

  (* Donne l'ensemble des litéraux qui apparaissent dans des clauses n'ayant qu'un seul wlit. *)
  let entail w = Stc.fold (fun c s -> let x = fst (watched c w) in
    if Cor.read x = 0 then St.add (x, c) s else s) (assoc 0 w) St.empty

  let fold = Stc.fold
  let choose = St.choose
  let singleton x = St.singleton (x, -1)
  let empty = St.empty
  let is_empty = St.is_empty
  let union = St.union
  let add = St.add
  let remove x = St.remove (x, 0)
  let elements = St.elements
end;;

module type WlitAbstract = functor (Cor: Core) -> 
sig
  type set = St.t
  type wlit
  type cls = Cor.cls
  val wempty : wlit
  val extract : int -> (cls list * cls list) -> wlit -> wlit
  val entail : wlit -> set
  val update_cls : cls -> wlit ->  wlit

end;;

module Make = (WlitCore: WlitAbstract);;

