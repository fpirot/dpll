(* Clause *)
(* Module d'implementation des clauses. *)

module type ClauseElt =
sig
  exception Satisfiable
  exception Unsatisfiable
  val cls : int
  val read : int -> int
  val write : int -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
end;;

module ClauseCore = functor (Elt : ClauseElt) ->
struct

  module Cls = Set.Make
    (struct
      type t = int
      let compare x y = compare (abs x) (abs y)
     end)
  (* Les clauses sont des ensembles d'entiers (+x pour le litéral
     vrai de la variable x, -x pour sa négation), avec la relation
     de comparaison sur les valeurs absolues (entre nom de
     variable). *)
    
  module St = Set.Make
    (struct
      type t = int
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
    
  let clauseArray = Array.make Elt.cls Cls.empty
  (* On référencie l'ensemble des clauses dans un tableau, afin de
     stocker des indices dans nos structures de données plutôt que des
     clauses. *)

  let clause id = clauseArray.(id)
    
  let compt = ref (-1)
  (* L'indice en cours dans le tableau. *)
    
  let debug = true
  let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l
    
  type cls = int
  type set = St.t
  type map = St.t Mp.t
      
  let empty = Mp.empty
  (* Table d'association vide. *)

  let cls_make id = id

  let length id = Cls.fold (fun x t -> if Elt.read x = 0 then t+1 else t) (clause id) 0

  let cls_fold f id a = Cls.fold f (clause id) a
    
  let fill l =
    incr compt;
    clauseArray.(!compt) <- Elt.fold (fun x s -> Cls.add x s) l Cls.empty;
    if debug then begin
    end;
    !compt
  (* Renvoie dans la case du tableau en cours la clause représentée par sa liste d'entiers l. *)
      
  let add id map =
    Cls.fold (fun x m -> let s = try Mp.find x m with _ -> St.empty in
			 Mp.add x (St.add id s) m) clauseArray.(id) map
  (* Ajoute la clause d'indice id dans la table d'association. *)
      
  let reset () =
    Array.fill clauseArray 0 (Elt.cls - 1) Cls.empty;
    compt := -1
  (* Réinitialise le tableau de clauses. *)
      
  let create lst =
    reset ();
    let m = Elt.fold (fun l m -> add (fill l) m) lst Mp.empty in
		  if debug then begin
		    print_string "clauseArray:\n";
		    Array.iteri (fun i x ->
		      print_int i;
		      print_string ": ";
		      print_list (Cls.elements x);
		      print_newline())
		      clauseArray;
		    print_newline()
		  end; m
      
  let is_empty = Mp.is_empty
  (* Teste si la table d'association est vide. *)

  let is_singleton id = 
    try (
      match Cls.fold 
	(fun x v -> match (Elt.read x, v) with
          |(0, 0) -> x
          |(0, _) -> failwith "is_not"
          |(_, y) -> y) clauseArray.(id) 0
      with
	|0 -> raise Elt.Unsatisfiable
	|x -> x
    )
    with Failure "is_not" -> 0

  (* Renvoie l'unique élément de la clause d'indice id qui n'est pas
     encore assigné quand il est bien unique, 0 sinon.  Lève
     l'exception Unsatisfiable si la clause n'est pas satisfiable.*)

  let mem = Mp.mem
  (* Indique si une variable est présente dans l'ensemble des
     clauses. *)

  let literals id = Cls.elements clauseArray.(id)
  (* Donne les elements d'une clause *)

  let remove id map =
(*    Cls.fold (fun x m -> try (Mp.add x (St.remove id (Mp.find x m)) m) with Not_found -> m) clauseArray.(id) map *)
    Cls.fold (fun x m -> let s = try (St.remove id (Mp.find x m)) with Not_found -> St.empty in
	     if s <> St.empty then Mp.add x s m else Mp.remove x m) clauseArray.(id) map
  (* Supprime une clause de la map *)

  let bindings m = let lst = Mp.bindings m in
		   List.map (fun (k, s) ->
		     (k, List.map (fun id -> Cls.elements clauseArray.(id)) (St.elements s))) lst
  (* Affichage des éléments de la table d'association sous forme de
     liste. *)
		     
  let elements id = Cls.elements clauseArray.(id)
  (* Affichage des éléments d'une clause sous forme de liste. *)

  (*let extract x map = let s = Mp.find x map and m = remove x map
    in (St.elements s, m)*)
    
  let extract x map = 
    let s = try Mp.find x map with _ -> St.empty in
    let m = St.fold (fun id m -> remove id m) s map in
    if debug then begin
      print_string "Extraction:\n";
      List.iter (fun x ->
	print_int x;
	print_string ": ";
	print_list (Cls.elements clauseArray.(x));
	print_newline())
        (St.elements s);
      print_newline ();
      print_string "New map:\n";
      List.iter 
	(fun (x, lst) ->
          if lst <> [] then begin
	    print_int x;
	    print_string ": ";
	    List.iter (fun l -> print_list l; print_char ' ') lst;
	    print_newline() end) (bindings (Mp.remove x m));
      print_newline();
    end;
    (St.elements s, Mp.remove x m)
      
  let choose id = Cls.choose clauseArray.(id)
    
  let find x m = try St.elements (Mp.find x m) with _ -> []
(* Renvoie la liste de toutes les clauses attachées à un litéral, et
   la table d'association privée de ces clauses et de la négation du
   litéral (lorsque l'on donne à une variable une assignation
   particulière). *)
    
end;;


module type ClauseAbstract = functor (Elt : ClauseElt) ->
sig
  type map
  type cls
  val empty : map
  val fill : int list -> cls
  val add : cls -> map -> map
  val create : int list list -> map
  val reset : unit -> unit
  val is_empty : map -> bool
  val is_singleton : cls -> int
  val mem : int -> map -> bool
  val literals : int -> int list
  val remove : cls -> map -> map
  val bindings : map -> (int * int list list) list
  val elements : cls -> int list
  val extract : int -> map -> cls list * map
  val choose : cls -> int
  val find : int -> map -> cls list
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val is_empty : map -> bool
end;;

module Make = (ClauseCore : ClauseAbstract);;

(* Tests *)
(*
module Test = Make
  (struct
    let cls = 10
    let tab = Array.create 10 0
    let read n = tab.(n-1)
    let write n x = tab.(n-1) <- x
    let fold = List.fold_right
  end);;

let s = Test.empty;; Test.bindings s;; 
let s = Test.create [[0; -1; 2]; [1; -2; 3]; [2; -3; 0]; [3; -0; 1]; [0; -1; -2]];; 
Test.bindings s;;
let (l, s) = Test.extract 1 s;; 
List.map (fun cls -> Test.elements cls) l;;
*)
