module type Core =
sig
  type cls = int
  val nb_cls : unit -> int
  val read : int -> int
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;


module MomsCore = functor (Cor: Core) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let debug = false

  let is_empty = Map.is_empty

  type order = int Map.t Map.t
  (* Table d'association, qui a v associe le nombre d'apparitions de la
     variable v dans les clauses, par une table d'association avec pour clé
     la taille de ces clauses. *)

  let remove = List.fold_right
    (fun c mp -> let n = Cor.length c in
		 Cor.cls_fold (fun x m -> let mx = try Map.find x m with Not_found -> Map.empty in
					  let v = (try Map.find n mx with Not_found -> 0) - 1 in
					  let mx' = if v > 0 then Map.add n v mx else Map.remove n mx in
					  if is_empty mx' then Map.remove x m else Map.add x mx' m) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map. *)

  let decr_size = List.fold_right
    (fun c mp -> let n = Cor.length c in
		 Cor.cls_fold (fun x m -> let mx = try Map.find x m with Not_found -> Map.empty in
					  let v1 = (try Map.find n mx with Not_found -> 0) - 1
					  and v2 = (try Map.find (n-1) mx with Not_found -> 0) + 1 in
					  let mx1 = if v1 > 0 then Map.add n v1 mx else Map.remove n mx in
					  Map.add x (Map.add (n-1) v2 mx1) m) c mp)
  (* Actualise la map lorsque l'on a une liste de clauses qui vont voir
     leur taille décrémenter d'une unité. *)

  let add = List.fold_right 
    (fun c mp -> let n = Cor.length c in
		 Cor.cls_fold (fun x m -> let mx = try Map.find x m with Not_found -> Map.empty in
					  Map.add x (Map.add n ((try Map.find n mx with Not_found -> 0) + 1) mx) m) c mp)
  (* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract ord =
    let length_min = Map.fold (fun x mx n -> let (p,_) = Map.min_binding mx in min p n) ord max_int in
    let xmoms = fst (Map.fold (fun x mx (xm, max) ->
      let p = try Map.find length_min mx with Not_found -> 0 in
      if (Cor.read x = 0 && p > max) then (x, p) else (xm, max)) ord (0,0)) in
    if xmoms = 0 then raise Not_found else xmoms
  (* Renvoie le choix xmoms selon l'ordre. *)

  let update x (l1, l2) ord =
    decr_size l2 (remove l1 (Map.remove (-x) (Map.remove x ord)))
  (* Met l'ordre à jour lorsque l'on affecte le litéral x à vrai. *)

  let is_empty = Map.is_empty

  let create () =
    let rec make_list l = function
      |0 -> Cor.cls_make 0 :: l
      |n -> make_list (Cor.cls_make (n-1) :: l) (n-1) in
    let l = make_list [] (Cor.nb_cls ()) in
    add l Map.empty
    
end;;

module type MomsAbstract = functor (Cor: Core) ->
sig
  type order
  val is_empty : order -> bool
  val create : unit -> order
  val extract : order -> int
  val update : int -> (Cor.cls list * Cor.cls list) -> order -> order
  val add : Cor.cls list -> order -> order
end;;

module Make = (MomsCore : MomsAbstract)
