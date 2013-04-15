(* Clause *)
(* Module d'implementation des clauses. *)

module type ClauseElt =
sig
  type cls = int
  val fill : int list -> cls
  val literals : cls -> int list
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val read : int -> int
  val write : ?father: int -> int -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
end;;

module ClauseCore = functor (Elt : ClauseElt) ->
struct

  type cls = Elt.cls
    
  module St = Set.Make
    (struct
      type t = cls
      let compare = compare
     end)
  (* Une structure d'ensemble d'entiers avec la comparaison
     habituelle. *)
    
  module Mp = Map.Make
    (struct
      type t = int
      let compare = compare
     end)
  (* On gère une table d'association qui à chaque litéral associe
     l'ensemble des indices de clauses qui le contiennent. *)

  let is_empty = Mp.is_empty
    
  let debug = false
  let print_list l =
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]\n"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  type set = St.t
  type map = St.t Mp.t
      
  let empty = Mp.empty
  (* Table d'association vide. *)
      
  let is_empty = Mp.is_empty
  (* Teste si la table d'association est vide. *)

  let mem = Mp.mem
  (* Indique si une variable est présente dans l'ensemble des
     clauses. *)

  let add cls map =
    Elt.fold (fun x m -> let s = try Mp.find x m with _ -> St.empty in
			 Mp.add x (St.add cls s) m) (Elt.literals cls) map
  (* Ajoute la clause d'indice id dans la table d'association. *)

  let create lst = List.fold_right (fun l m -> add (Elt.fill l) m) lst Mp.empty
  (* Crée une map à partir d'une liste de clauses (sous forme de listes).
     Remplit au passage le tableau de clauses dans Core. *)

  let remove cls map =
    Elt.fold (fun x m -> let s = try (St.remove cls (Mp.find x m)) with Not_found -> St.empty in
	     if s <> St.empty then Mp.add x s m else Mp.remove x m) (Elt.literals cls) map
  (* Supprime une clause de la map *)

  let bindings m = let lst = Mp.bindings m in
		   List.map (fun (k, s) ->
		     (k, List.map (fun cls -> Elt.literals cls) (St.elements s))) lst
  (* Affichage des éléments de la table d'association sous forme de
     liste. *)

  let extract x map = 
    let s = try Mp.find x map with _ -> St.empty in
    let m = St.fold (fun cls m -> remove cls m) s map in
    if debug then begin
      print_string "Extraction of "; print_int x; print_string ":\n";
      List.iter (fun x -> print_list (Elt.literals x)) (St.elements s) end;
    Mp.remove x m

  let find x m = try St.elements (Mp.find x m) with _ -> []
(* Renvoie la liste de toutes les clauses attachées à un litéral. *)

end;;


module type ClauseAbstract = functor (Elt : ClauseElt) ->
sig
  type map
  type cls = Elt.cls
  val empty : map
  val add : cls -> map -> map
  val create : int list list -> map
  val is_empty : map -> bool
  val mem : int -> map -> bool
  val remove : cls -> map -> map
  val bindings : map -> (int * int list list) list
  val extract : int -> map -> map
  val find : int -> map -> cls list
end;;

module Make = (ClauseCore : ClauseAbstract);;
