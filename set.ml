exception Satisfiable;;
exception Unsatisfiable;;

(* Set *)
(* Implementation du module d'entree pour le module Clause. 
Les fonctions ont le comportement decrit dans le module type du fichier clause. *)

module type SetElt =
  sig
    val nbr : int
    val fold : (int -> 'b -> 'b) -> int list -> 'b -> 'b
  end;;

module SetCore = functor (Elt : SetElt) ->
  struct

    let b = ref true
    let compt = ref (-1)

    module Cls = Set.Make
      (struct
        type t = int
        let compare x y = match compare (abs x) (abs y) with
          |0 -> if x + y = 0 then b := false else b := true; 0
          |n -> n
      end)

    module Ens = Map.Make
      (struct
        type t = int
        let compare x y = compare (abs x) (abs y)
      end)

    let clauseArray = Array.create Elt.nbr Cls.empty



    type set = int
    type map = int list Ens.t


    let empty = Ens.empty

    let create l =
      incr compt;
      clauseArray.(!compt) <- Elt.fold (fun x s -> Cls.add x s) l Cls.empty;
      !compt

    let reset () =
      Array.fill clauseArray 0 (Elt.nbr - 1) Cls.empty;
      compt := -1

    let is_empty = Ens.is_empty

    let is_unsat id = Cls.is_empty clauseArray.(id)

    let mem = Ens.mem

    let add id map =
      Cls.fold (fun x m -> let l = try Ens.find x m with _ -> [] in
        Ens.add x (id::l) m) clauseArray.(id) map

    let variable x id =
      clauseArray.(id) <- Cls.remove x clauseArray.(id); (!b, id)

    let remove x map =
      List.iter (fun id -> clauseArray.(id) <- Cls.remove x clauseArray.(id))
        (Ens.find x map);
      Ens.remove x map

    let bindings m = let lst = Ens.bindings m in
      List.map (fun (k, l) ->
        (k, List.map (fun id -> Cls.elements clauseArray.(id)) l)) lst

    let extract x map =
      let l = Ens.find x map
      and m = Ens.remove x map in (l, m)

  end;;

module type SetAbstract = functor (Elt : SetElt) ->
  sig
    type map
    type set
    val empty : map
    val create : int list -> set
    val reset : unit -> unit
    val is_empty : map -> bool
    val is_unsat : set -> bool
    val mem : int -> map -> bool
    val add : set -> map -> map
    val variable : int -> set -> bool * set
    val remove : int -> map -> map
    val bindings : map -> (int * int list list) list
    val extract : int -> map -> set list * map
  end;;

module Set = (SetCore : SetAbstract);;

(* Tests *)


module SetTest = Set
  (struct
    type t = int list
    let nbr = 10
    let fold = List.fold_right
  end);;

let s = SetTest.empty;;
SetTest.bindings s;;
let s = SetTest.add (SetTest.create [1; -2; 2]) s;;
let s = SetTest.add (SetTest.create [3;5]) s;;
let s = SetTest.add (SetTest.create [-4;2]) s;;
let s = SetTest.add (SetTest.create [1;2]) s;;
let s = SetTest.add (SetTest.create [6;-2]) s;;
let s = SetTest.add (SetTest.create [0;-6]) s;;
let s = SetTest.add (SetTest.create [2]) s;;
SetTest.bindings s;;
