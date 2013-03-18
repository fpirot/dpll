module type Clause =
sig
  type t
  type elt = int
  val length : t -> int
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end;;

module DlisCore = functor (Elt : Clause) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  type map = float Map.t
  (* Table d'association, qui a x associe le score obtenu par ce litéral. *)

  let rec power2 = function
    |0 -> 1.
    |n -> let x = power2 (n/2) in
	  if n > 0 then float ((n mod 2) + 1) *. x *. x
	  else x *. x /. float (abs (n mod 2) + 1);;
  (* Calcule les puissances entières (y compris négatives) de 2. *)

  let remove = List.fold_right 
    (fun c mp -> let n = Elt.length c in let v = power2 (-n) in
		 Elt.fold (fun x m -> try Map.add x ((Map.find x m) -. v) m
                                      with Not_found -> m) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map. *)

  let add = List.fold_right 
    (fun c mp -> let n = Elt.length c in let v = power2 (-n) in
		 Elt.fold (fun x m -> Map.add x ((try Map.find x m with Not_found -> 0.) +. v) m) c mp)
(* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract mp =
    let xdlis = fst (Map.fold (fun x v (xm, max) -> if v > max then (x,v) else (xm, max)) mp (0,0.)) in
    (xdlis, Map.remove xdlis (Map.remove (-xdlis) mp))
(* Renvoie le couple (xmoxs, map), avec xmoms le litéral choisi par
   l'heuristique MOMS, et map la table d'association privée de la
   variable corresopndante. *)
end;;

module type DlisAbstract = functor (Elt : Clause) ->
sig
  type map
  val remove : Elt.t list -> map -> map
  val add : Elt.t list -> map -> map
  val extract : map -> int * map
end;;

module Make = (DlisCore : DlisAbstract)
