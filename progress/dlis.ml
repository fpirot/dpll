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
  val read : int -> int
  val var : int
end;;

module DlisCore = functor (Elt: Clause) -> functor (Cor: Core) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  type order = float Map.t
  (* Table d'association, qui a x associe le score obtenu par ce litéral. *)

  let rec power2 = function
    |0 -> 1.
    |n -> let x = power2 (n/2) in
	  if n > 0 then float ((n mod 2) + 1) *. x *. x
	  else x *. x /. float (abs (n mod 2) + 1);;
  (* Calcule les puissances entières (y compris négatives) de 2. *)

  let remove = List.fold_right 
    (fun c mp -> let n = Elt.length c in let v = power2 (-n) in
		 Elt.cls_fold (fun x m -> try Map.add x ((Map.find x m) -. v) m
                                      with Not_found -> m) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map. *)

  let decr_size = List.fold_right 
    (fun c mp -> let n = Elt.length c in    
		 Elt.cls_fold (fun x m -> try (let v = Map.find x m in
					       let v1 = power2 (-n-1) in
					       Map.add x (v +. v1) m)
		   with Not_found -> m) c mp)
  (* Prend en compte une liste de clauses dont la taille a été
     décrémentée. *)

  let add = List.fold_right 
    (fun c mp -> let n = Elt.length c in let v = power2 (-n) in
					 Elt.cls_fold (fun x m -> Map.add x ((try Map.find x m with Not_found -> 0.) +. v) m) c mp)
  (* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract mp =
    let xdlis = fst (Map.fold (fun x v (xm, max) -> if (Cor.read x = 0 && v > max) then (x,v) else (xm, max)) mp (0,0.)) in
    if xdlis = 0 then raise Cor.Satisfiable
    else (xdlis, Map.remove xdlis (Map.remove (-xdlis) mp))
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

module type DlisAbstract = functor (Elt: Clause) -> functor (Cor: Core) ->
sig
  type order
  val remove : Elt.cls list -> order -> order
  val decr_size : Elt.cls list -> order -> order
  val extract : order -> int * order
  val is_empty : order -> bool
  val create : unit -> order
end;;

module Make = (DlisCore : DlisAbstract)
