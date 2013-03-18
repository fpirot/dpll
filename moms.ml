module type Clause =
sig
  type cls
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
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

  let add = List.fold_right 
    (fun c mp -> let n = Elt.length c in
		 Elt.cls_fold (fun x m -> let mx = try Map.find x m with Not_found -> Map.empty in
				      Map.add x (Map.add n ((try Map.find n mx with Not_found -> 0) + 1) mx) m) c mp)
(* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract mp = 
    let length_min = Map.fold (fun x mx n -> let (p,_) = Map.min_binding mx in min p n) mp (fst (Map.choose mp)) in
    let xmoms = fst (Map.fold (fun x mx (xm, max) ->
      let p = try Map.find length_min mx with Not_found -> 0 in if (Cor.read x = 0 && p > max) then (x, p) else (xm, max)) mp (0,0)) in
    if xmoms = 0 then raise Cor.Satisfiable
    else (xmoms, Map.remove xmoms (Map.remove (-xmoms) mp))
(* Renvoie le couple (xmoxs, map), avec xmoms le litéral choisi par
   l'heuristique MOMS, et map la table d'association privée de la
   variable corresopndante. *)

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
  val remove : Elt.cls list -> order -> order
  val add : Elt.cls list -> order -> order
  val extract : order -> int * order
  val is_empty : order -> bool
  val create : unit -> order
end;;

module Make = (MomsCore : MomsAbstract)
