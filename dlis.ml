module type Core =
sig
  exception Satisfiable
  type cls = int
  val cls : int
  val read : int -> int
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;

module type Clause =
sig
  type cls
  type map
  val find : int -> map -> cls list
end;;


module DlisCore = functor (Cor: Core) -> functor (Elt: Clause with type cls = Cor.cls) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let debug = false

  type order = float Map.t
  (* Table d'association, qui a x associe le score obtenu par ce litéral. *)

  let rec power2 = function
    |0 -> 1.
    |n -> let x = power2 (n/2) in
	  if n > 0 then float ((n mod 2) + 1) *. x *. x
	  else x *. x /. float (abs (n mod 2) + 1);;
  (* Calcule les puissances entières (y compris négatives) de 2. *)

  let is_empty = Map.is_empty

  let remove = List.fold_right 
    (fun c mp -> let n = Cor.length c in let v = power2 (-n) in
		 Cor.cls_fold (fun x m -> let v1 = (try Map.find x m with Not_found -> 0.) -. v in
					  if v1 > 0. then Map.add x v1 m else Map.remove x m) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map. *)

  let decr_size = List.fold_right 
    (fun c mp -> let n = Cor.length c in
		 Cor.cls_fold (fun x m -> let v = try Map.find x m with Not_found -> 0. in
					  let v1 = if v > 0. then v +. power2 (-n) else power2 (1-n) in
					  Map.add x v1 m) c mp)
  (* Prend en compte une liste de clauses dont la taille va être décrémentée. *)

  let add = List.fold_right
    (fun c mp -> let n = Cor.length c in let v = power2 (-n) in
		 Cor.cls_fold (fun x m -> Map.add x ((try Map.find x m with Not_found -> 0.) +. v) m) c mp)
  (* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract map ord =
    if debug then begin
      print_string "Order:\n";
      if is_empty ord then print_string "is empty.\n";
      List.iter (fun (x,y) -> print_int x; print_string ": "; print_float y; print_string "; ") (Map.bindings ord);
      print_string "\n"
    end;
    let xdlis = fst (Map.fold (fun x v (xm, max) -> if (Cor.read x = 0 && v > max) then (x,v) else (xm, max)) ord (0,0.)) in
    if xdlis = 0 then raise Cor.Satisfiable
    else 
      let l1 = Elt.find xdlis map 
      and l2 = Elt.find (-xdlis) map in
      (xdlis, decr_size l2 (remove l1 (Map.remove xdlis (Map.remove (-xdlis) ord))))
(* Renvoie le couple (xmoxs, map), avec xmoms le litéral choisi par
   l'heuristique DLIS, et map la table d'association privée de la
   variable corresopndante. *)

  let update x map ord = 
    let l1 = try Elt.find x map with Not_found -> []
    and l2 = try Elt.find (-x) map with Not_found -> [] in
    decr_size l2 (remove l1 (Map.remove (-x) (Map.remove x ord)))
  (* Met l'ordre à jour lorsque l'on affecte le litéral x à vrai. *)

  let create () =
    let rec make_list l = function
      |0 -> Cor.cls_make 0 :: l
      |n -> make_list (Cor.cls_make (n-1) :: l) (n-1) in
    let l = make_list [] Cor.cls in
    add l Map.empty
end;;

module type DlisAbstract = functor (Cor: Core) -> functor (Elt: Clause with type cls = Cor.cls) ->
sig
  type order
  val is_empty : order -> bool
  val create : unit -> order
  val extract : Elt.map -> order -> int * order
  val update : int -> Elt.map -> order -> order
end;;

module Make = (DlisCore : DlisAbstract)
