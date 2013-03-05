(* Ceci est un commentaire inutile *)

exception Satisfiable;;
exception Unsatisfiable;;

module type AssigElt =
  sig
    type t
    val zero : t
    val nbr : int
  end;;

module AssigCore = functor (Elt : AssigElt) ->
  struct
    type t = Elt.t
    let assigArray = Array.create Elt.nbr Elt.zero
    let read n = assigArray.(n - 1)
    let write n x = assigArray.(n - 1) <- x
  end;;

module type AssigAbstract = functor (Elt : AssigElt) ->
  sig
    type t = Elt.t
    val read : int -> t
    val write : int -> t -> unit
  end;;

module Assig = (AssigCore : AssigAbstract);;


(* Order *)

module OrderCore =
  struct
    type order = (int * int) list
    let hd l = snd (List.hd l)
    let tl = List.tl
    let init l = List.map (fun x -> x, x) l
  end;;

module type OrderAbstract =    sig
    type order
    val hd : order -> int
    val tl : order -> order
    val init : int list -> order
  end;;

module Order = (OrderCore: OrderAbstract);;



(* Set *)


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




(* Clause *)


module type ClauseElt =
  sig
    type set
    type map
    val empty : map
    val is_empty : map -> bool
    val is_unsat : set -> bool
    val mem : int -> map -> bool
    val add : set -> map -> map
    val variable : int -> set -> bool * set
    val remove : int -> map -> map
    val bindings : map -> (int * int list list) list
    val extract : int -> map -> set list * map
  end ;;

module type OrdElt =
  sig
    type order
    val hd : order -> int
    val tl : order -> order
  end;;



module ClauseCore = functor (Elt : ClauseElt) -> functor (Ord : OrdElt) ->
  struct
    type env = {clause: Elt.map; order: Ord.order}
    type set = Elt.set
    type map = Elt.map


    let split env =
      let k = Ord.hd env.order in
      let (l, m) = Elt.extract k env.clause in
        ((k, l),
          {clause = m;
            order = Ord.tl env.order})
    
    
    let assignment (x, lstC) =
    
      let rec assigC optT optF = function
        |[] -> optT, optF
        |c::l -> match optT, optF with
          | None, None -> failwith "Assignment"
          | Some lstT, None -> begin match assigV c with
              | true, _ -> assigC optT optF l
              | false, None -> raise Unsatisfiable
              | false, Some v -> assigC (Some (v :: lstT)) None l
            end
          | None, Some lstF -> begin match assigV c with
              | false, _ -> assigC optT optF l
              | true, None -> raise Unsatisfiable
              | true, Some v -> assigC None (Some (v :: lstF)) l
            end
          | Some lstT, Some lstF -> begin match assigV c with
              | false, None -> assigC None  (Some lstF) l
              | true, None -> assigC (Some lstT) None l
              | false, Some v -> assigC (Some (v :: lstT)) optF l
              | true, Some v -> assigC optT (Some (v :: lstF)) l
            end
      
      and assigV c =
        let (b, c) = Elt.variable x c in
          if Elt.is_unsat c then b, None else b, Some c         
      
    in assigC (Some []) (Some []) lstC
     
  end;;


module type ClauseAbstract = functor (Elt : ClauseElt) -> functor (Ord : OrdElt) ->
  sig
    type env
    type set
    type map
    val split : env -> (int * set list) * env
    val assignment : int * set list -> set list option * set list option
  end;;


module Clause = (ClauseCore : ClauseAbstract);;

(* Test *)

module SetTest = Set
  (struct
    type t = int list
    let nbr = 10
    let fold = List.fold_right
  end);;

module ClauseTest = ClauseCore (SetTest) (Order);;

let s = SetTest.empty;;
let s = SetTest.add (SetTest.create [0;1;2]) s;;
let s = SetTest.add (SetTest.create [2;3]) s;;
let s = SetTest.add (SetTest.create [0;4]) s;;
let s = SetTest.add (SetTest.create [1;2;3]) s;;
let s = SetTest.add (SetTest.create [0;2;4]) s;;
SetTest.bindings s;;
