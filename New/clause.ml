(* Clause *)
(* Module d'implementation des clauses. *)

module type ClauseElt =
  sig
    val cls : int
    val read : int -> int
    val write : int -> int -> unit
    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  end;;

module ClauseCore = functor (Elt : ClauseElt) ->
  struct
    module Cls = Set.Make
      (struct
        type t = int
        let compare x y = compare (abs x) (abs y)
      end)
    (* Les clauses sont des ensembles d'entiers (+x pour le litéral vrai de la variable x, -x pour sa négation), 
       avec la relation de comparaison sur les valeurs absolues (entre nom de variable). *)
    
    module St = Set.Make
      (struct
        type t = int
        let compare = compare
      end)
    (* Une structure d'ensemble d'entiers avec la comparaison habituelle. *)
    
    module Mp = Map.Make
      (struct
        type t = int
        let compare = compare
      end)
    (* On gère une table d'association qui à chaque litéral associe l'ensemble des indices de clauses qui le contiennent. *)
    
    let clauseArray = Array.make Elt.cls Cls.empty
    (* On référencie l'ensemble des clauses dans un tableau, afin de stocker des indices dans nos structures de données plutôt que des clauses. *)
    
    let compt = ref (-1)
    (* L'indice en cours dans le tableau. *)
    
    type cls = int
    type set = St.t
    type map = St.t Mp.t
    
    let empty = Mp.empty
    (* Table d'association vide. *)
    
    let fill l =
      incr compt;
      clauseArray.(!compt) <- Elt.fold (fun x s -> Cls.add x s) l Cls.empty;
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
      Elt.fold (fun l m -> add (fill l) m) lst Mp.empty
    
    let is_empty = Mp.is_empty
    (* Teste si la table d'association est vide. *)

    let are_sat id = Cls.fold (fun x c -> if Elt.read x = 0 then c+1 else c) clauseArray.(id) 0
    (* Renvoie le nombre de litéraux dont on ne connaît pas encore l'assignation dans la clause. *)

    let mem = Mp.mem
    (* Indique si une variable est présente dans l'ensemble des clauses. *)

    let variable x id =
      clauseArray.(id) <- Cls.remove x clauseArray.(id); id
    (* Retire de la clause d'indice id les litéraux correspondant à la variable x. *)

    let remove id map =
      Cls.fold (fun x m -> Mp.add x (St.remove id (Mp.find x m)) m) clauseArray.(id) map
    (* Supprime une clause de la map *)

    let bindings m = let lst = Mp.bindings m in
      List.map (fun (k, s) ->
        (k, List.map (fun id -> Cls.elements clauseArray.(id)) (St.elements s))) lst
    (* Affichage des éléments de la table d'association sous forme de liste. *)
    
    let elements id = Cls.elements clauseArray.(id)
    (* Affichage des éléments d'une clause sous forme de liste. *)

    (*let extract x map =
      let s = Mp.find x map
      and m = remove x map in (St.elements s, m)*)
    
    let extract x map = 
      let s = Mp.find x map in
      let m = St.fold (fun id m -> remove id m) s map in
        (St.elements s, Mp.remove x m)
    
    let choose id = Cls.choose clauseArray.(id)
    
    let find x m = St.elements (Mp.find x m)

    (* Renvoie la liste de toutes les clauses attachées à un litéral, et la table d'association privée de ces clauses et de la négation du litéral (lorsque l'on donne à une variable une assignation particulière). *)
    
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
    val are_sat : cls -> int
    val mem : int -> map -> bool
    val variable : int -> cls -> cls
    val remove : cls -> map -> map
    val bindings : map -> (int * int list list) list
    val elements : cls -> int list
    val extract : int -> map -> cls list * map
    val choose : cls -> int
    val find : int -> map -> cls list
  end;;

module Make = (ClauseCore : ClauseAbstract);;

(* Tests *)

module Test = Make
  (struct
    let cls = 10
    let tab = Array.create 10 0
    let read n = tab.(n-1)
    let write n x = tab.(n-1) <- x
    let fold = List.fold_right
  end);;

let s = Test.empty;;
Test.bindings s;;
let s = Test.create [[0; -1; 2]; [1; -2; 3]; [2; -3; 0]; [3; -0; 1]; [0; -1; -2]];;
Test.bindings s;;
let (l, s) = Test.extract 1 s;;
List.map (fun cls -> Test.elements cls) l;;
Test.bindings s;;
