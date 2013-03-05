exception Satisfiable;;
exception Unsatisfiable;;

(* Set *)
(* Implementation du module d'entree pour le module Clause. 
Les fonctions ont le comportement decrit dans le module type du fichier clause. *)

module type ClauseElt =
  sig
    val nbr : int
    val fold : (int -> 'b -> 'b) -> int list -> 'b -> 'b
  end;;

module ClauseCore = functor (Elt : ClauseElt) ->
  struct
    module Cls = Set.Make
      (struct
        type t = int
        let compare x y = compare (abs x) (abs y)
      end)
    
    module St = Set.Make
      (struct
        type t = int
        let compare = compare
      end)
    
    module Mp = Map.Make
      (struct
        type t = int
        let compare = compare
      end)
    
    let clauseArray = Array.make Elt.nbr Cls.empty
    
    let compt = ref (-1)
    
    type cls = int
    type set = St.t
    type map = St.t Mp.t
    
    
    let empty = Mp.empty
    
    let create l =
      incr compt;
      clauseArray.(!compt) <- Elt.fold (fun x s -> Cls.add x s) l Cls.empty;
      !compt
    
    let reset () =
      Array.fill clauseArray 0 (Elt.nbr - 1) Cls.empty;
      compt := -1
    
    let is_empty = Mp.is_empty

    let is_unsat id = Cls.is_empty clauseArray.(id)

    let mem = Mp.mem

    let add id map =
      Cls.fold (fun x m -> let s = try Mp.find x m with _ -> St.empty in
        Mp.add x (St.add id s) m) clauseArray.(id) map

    let variable x id =
      clauseArray.(id) <- Cls.remove x clauseArray.(id); id

    let remove x map =
      St.iter (fun id -> clauseArray.(id) <- Cls.remove x clauseArray.(id))
        (Mp.find x map);
      Mp.remove x map

    let bindings m = let lst = Mp.bindings m in
      List.map (fun (k, s) ->
        (k, List.map (fun id -> Cls.elements clauseArray.(id)) (St.elements s))) lst
    
    let elements id = Cls.elements clauseArray.(id)

    let extract x map =
      let s = Mp.find x map
      and m = Mp.remove x map in (St.elements s, m)
    
  end;;


module type ClauseAbstract = functor (Elt : ClauseElt) ->
  sig
    type map
    type cls
    val empty : map
    val create : int list -> cls
    val reset : unit -> unit
    val is_empty : map -> bool
    val is_unsat : cls -> bool
    val mem : int -> map -> bool
    val add : cls -> map -> map
    val variable : int -> cls -> cls
    val remove : int -> map -> map
    val bindings : map -> (int * int list list) list
    val elements : cls -> int list
    val extract : int -> map -> cls list * map
  end;;

module Make = (ClauseCore : ClauseAbstract);;

(* Tests *)

module Test = Make
  (struct
    let nbr = 10
    let fold = List.fold_right
  end);;

let s = Test.empty;;
Test.bindings s;;
let s = Test.add (Test.create [0; -1; 2]) s;;
let s = Test.add (Test.create [1; -2; 3]) s;;
let s = Test.add (Test.create [2; -3; 0]) s;;
let s = Test.add (Test.create [3; -0; 1]) s;;
let s = Test.add (Test.create [0; -1; -2]) s;;
Test.bindings s;;
let s = Test.remove 0 s;;
Test.bindings s;;
let (l, s) = Test.extract 1 s;;
List.map (fun cls -> Test.elements cls) l;;
Test.bindings s;;
