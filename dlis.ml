module type Core =
sig
  type cls = int
  val nb_cls : unit -> int
  val read : int -> int
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;


module DlisCore = functor (Cor: Core) ->
struct
  module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let debug = false

  let print_order ord = print_string "Order:\n";
    Map.iter (fun x v -> print_int x; print_string ": "; print_float v; print_newline()) ord

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
    (fun c mp -> let n = (Cor.length c) + 1 in let v = power2 (-n-1) in
		 Cor.cls_fold (fun x m -> let v1 = (try (Map.find x m) -. v  with Not_found -> 0.) -. v in
					  (*if v1 > 0. then*) Map.add x v1 m (*else Map.remove x m*)) c mp)
  (* Retire une liste de clauses à prendre en considération dans la map
     (il faut se rappeler que ces clauses viennent de se voir attribuer 
     un literal à vrai, et donc que leur taille a diminué d'un cran) *)

  let decr_size = List.fold_right 
    (fun c mp -> let n = Cor.length c in let v = power2 (-n-1) in
		 Cor.cls_fold (fun x m -> let v1 = try (Map.find x m) +. v with Not_found -> 0. in
					  (*if v1 > 0. then*) Map.add x v1 m (*else m*)) c mp)
  (* Prend en compte une liste de clauses dont la taille vient d'être décrémentée. *)

  let add = List.fold_right
    (fun c mp -> let n = Cor.length c in let v = power2 (-n) in
		 Cor.cls_fold (fun x m -> Map.add x ((try Map.find x m with Not_found -> 0.) +. v) m) c mp)
  (* Ajoute une liste de clauses à prendre en considération dans la map. *)

  let extract ord = if debug then print_order ord;
    let xdlis = fst (Map.fold (fun x v (xm, max) -> if (Cor.read x = 0 && v > max) then (x,v) else (xm, max)) ord (0,0.)) in
    if xdlis = 0 then raise Not_found else xdlis
  (* Renvoie le choix xdlis selon l'ordre. *)

  let update x (l1, l2) ord =
    let ord = decr_size l2 (remove l1 (Map.remove (-x) (Map.remove x ord))) in
    if debug then print_order ord; ord
  (* Met l'ordre à jour lorsque l'on affecte le litéral x à vrai. *)

  let create () =
    let rec make_list l = function
      |0 -> l
      |n -> make_list (Cor.cls_make (n-1) :: l) (n-1) in
    let l = make_list [] (Cor.nb_cls ()) in
    add l Map.empty
end;;

module type DlisAbstract = functor (Cor: Core) ->
sig
  type order
  val is_empty : order -> bool
  val create : unit -> order
  val add : Cor.cls list -> order -> order
  val extract : order -> int
  val update : int -> (Cor.cls list * Cor.cls list) -> order -> order
end;;

module Make = (DlisCore : DlisAbstract)
