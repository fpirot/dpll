module type Clause =
sig
  type map
  type cls
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val find : int -> map -> cls list
end;;

module type Core =
sig
  exception Satisfiable
  val var : int
  val read : int -> int
end;;

module MomsCore = functor (Elt: Clause) -> functor (Cor: Core) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  type order = int Map.t Map.t
  (* Table d'association, qui a v associe le nombre d'apparitions de la
     variable v dans les clauses, par une table d'association avec pour clé
     la taille de ces clauses. *)

  let remove = List.fold_right 
    (fun c mp -> let n = Elt.length c in    
		 Elt.cls_fold (fun x m -> try (let mx = Map.find x m in
					   Map.add x (Map.add n ((Map.find n mx) - 1) mx) m)
		   with Not_found -> m) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map. *)

  let decr_size = List.fold_right 
    (fun c mp -> let n = Elt.length c in    
		 Elt.cls_fold (fun x m -> try (let mx = Map.find x m in
					       let v1 = (Map.find n mx) - 1
					       and v2 = (Map.find (n-1) mx) + 1 in
					       let mx1 = Map.add n v1 mx in
					       Map.add x (Map.add (n-1) v2 mx1) m)
					       with Not_found -> m) c mp)
(* Actualise la map lorsque l'on a une liste de clauses qui vont voir leur taille décrémenter d'une unité. *)

  let add = List.fold_right 
    (fun c mp -> let n = Elt.length c in
		 Elt.cls_fold (fun x m -> let mx = try Map.find x m with Not_found -> Map.empty in
				      Map.add x (Map.add n ((try Map.find n mx with Not_found -> 0) + 1) mx) m) c mp)
(* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract map ord = 
    let length_min = Map.fold (fun x mx n -> let (p,_) = Map.min_binding mx in min p n) ord (fst (Map.choose ord)) in
    let xmoms = fst (Map.fold (fun x mx (xm, max) ->
      let p = try Map.find length_min mx with Not_found -> 0 in if (Cor.read x = 0 && p > max) then (x, p) else (xm, max)) ord (0,0)) in
    if xmoms = 0 then raise Cor.Satisfiable
    else 
      let l1 = Elt.find xmoms map 
      and l2 = Elt.find (-xmoms) map in
      (xmoms, decr_size l2 (remove l1 (Map.remove xmoms (Map.remove (-xmoms) ord))))
  (* Renvoie le couple (xmoxs, map), avec xmoms le litéral choisi par
     l'heuristique MOMS, et map la table d'association privée de la
     variable corresopndante. *)

  let update x map ord = 
    let l1 = Elt.find x map 
    and l2 = Elt.find (-x) map in
    decr_size l2 (remove l1 (Map.remove (-x) (Map.remove x ord)))
  (* Met l'ordre à jour lorsque l'on affecte le litéral x à vrai. *)

  let is_empty = Map.is_empty

  let create () =
    let rec make_list l = function
      |0 -> Elt.cls_make 0 :: l
      |n -> make_list (Elt.cls_make (n-1) :: l) (n-1) in
    let l = make_list [] Cor.var in
    add l Map.empty
    
end;;

module type MomsAbstract = functor (Elt: Clause) -> functor (Cor: Core) ->
sig
  type order
  val is_empty : order -> bool
  val create : unit -> order
  val extract : Elt.map -> order -> int * order
  val update : int -> Elt.map -> order -> order
end;;

module Make = (MomsCore : MomsAbstract)
