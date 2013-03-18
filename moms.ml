module type Clause =
sig
  type t
  type elt = int
  val length : t -> int
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end;;

module MomsCore = functor (Elt : Clause) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  type map = int Map.t Map.t
  (* Table d'association, qui a v associe le nombre d'apparitions de la
     variable v dans les clauses, par une table d'association avec pour clé
     la taille de ces clauses. *)

  let remove = List.fold_right 
    (fun c mp -> let n = Elt.length c in    
		 Elt.fold (fun x m -> try (let mx = Map.find x m in
					   Map.add x (Map.add n ((Map.find n mx) - 1) mx) m)
		   with Not_found -> m) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map. *)

  let add = List.fold_right 
    (fun c mp -> let n = Elt.length c in
		 Elt.fold (fun x m -> let mx = try Map.find x m with Not_found -> Map.empty in
				      Map.add x (Map.add n ((try Map.find n mx with Not_found -> 0) + 1) mx) m) c mp)
(* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract mp = 
    let length_min = Map.fold (fun x mx n -> let (p,_) = Map.min_binding mx in min p n) mp (fst (Map.choose mp)) in
    let xmoms = fst (Map.fold (fun x mx (xm, max) ->
      let p = try Map.find length_min mx with Not_found -> 0 in if p > max then (x, p) else (xm, max)) mp (0,0)) in
    (xmoms, Map.remove xmoms (Map.remove (-xmoms) mp))
(* Renvoie le couple (xmoxs, map), avec xmoms le litéral choisi par
   l'heuristique MOMS, et map la table d'association privée de la
   variable corresopndante. *)

  let is_empty = Map.is_empty
end;;

module type MomsAbstract = functor (Elt : Clause) ->
sig
  type map
  val remove : Elt.t list -> map -> map
  val add : Elt.t list -> map -> map
  val extract : map -> int * map
end;;

module Make = (MomsCore : MomsAbstract)
