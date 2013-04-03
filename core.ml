(* Module de, surprise, chargement des donnees *)

module Load =
struct

  exception Useless

  let rec insert x = function
    | [] -> [(0, x)]
    | (m, y) :: l ->
      if x = y then
	(m + 1, y) :: l
      else
	match insert x l with
	  | [] -> failwith "Insert"
	  | (m', y') :: l' ->
	    if m' > m then
	      (m', y') :: (m, y) :: l'
	    else
	      (m, y) :: (m', y') :: l'


  let add x l = if List.exists (fun y -> y = -x) l then raise Useless else x::l


  let rec read s channel = Scanf.bscanf channel "%0c"
    (fun c -> if c = 'c' then
	read (Scanf.bscanf channel "%[^\n]\n" (fun x -> s^ x ^ "\n")) channel
      else s)


  let init str n channel =
    let rec next () = match Scanf.bscanf channel " %d " (fun x -> x) with
      | 0 -> ()
      | n -> next ()
	
    and iter s c lst lstC ensV = function
      | 0 -> let s = read s channel in Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then (c + 1, ensV :: lstC, lst, s)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) 0 with
		       Useless -> (c, lstC, lst, s) end)
      | n -> let s = read s channel in Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then
		   iter s c lst (ensV :: lstC) [] (n - 1)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) n with
		       Useless ->
			 (next (); iter s (c - 1) lst lstC [] (n - 1)) end) in
    
    iter str n [] [] [] n;;


  let load channel =
    let s = read "" channel (*try Scanf.bscanf channel "c %s@\n" (fun x -> x ^ "\n") with _ -> ""*) in
    Scanf.bscanf channel "p cnf %d %d" (fun v c -> (v, init s (c-1) channel))

end;;


(* Regroupe les modules d'initialisation *)

module Core =
struct

  exception Satisfiable
  exception Unsatisfiable

  let debug = false

  let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  let (wlit, heur, path) =
    let w = ref false
    and s = ref "Nil"
    and p = ref "Test/ex0.cnf" in
    Arg.parse [("-wlit", Arg.Unit(fun () -> w := true), "Watched literals");
               ("-rand", Arg.Unit(fun () -> s := "Rand"), "Random selection");
               ("-moms", Arg.Unit(fun () -> s := "Moms"), "Maximum Occurrences in clauses of Minimum Size");
               ("-dlis", Arg.Unit(fun () -> s := "Dlis"), "Dynamic Largest Individual Sum")]
      (fun str -> p := str) "";
    (!w, !s, !p)
      
  let (var, (cls, lst, ord, comment)) = Load.load (Scanf.Scanning.open_in (path))
    
  let fold = List.fold_right

  type cls = int
  (* On repère une clause par son indice dans clauseArray. *)

  (* ********************************************************* *)
  (*        Gestion des affectations et des backtracks         *)
  (* ********************************************************* *)
      
  let stack = Array.make var []
  (* La "pile" contenant les assignations successives : on utilise un
     tableau de liste en partant du principe qu'il y a au plus n étages
     de paris, avec n le nombre de variables. *)

  let dpth = ref 0

  let fix_depth i = dpth := i;
    if debug then begin
      print_string "Stack: ";
      for k = 0 to i-1 do print_list stack.(k) done;
      print_newline() end

  type assig = {mutable value: int; mutable father: cls; mutable depth: int}
  (* Le type assig contient comme information la valeur de vérité
     value prise par le litéral (0 si indéfinie), la clause qui a
     engendré son assignation (-1 si c'est un pari), et la profondeur
     depth de son assignation dans la pile de propagation. *)

  let zero = {value = 0; father = -1; depth = 0}

  let assigArray = let t = Array.make var zero in
		   for i = 0 to var-1 do t.(i) <- {value = 0; father = -1; depth = 0} done; t
		     
  let reset x = assigArray.((abs x) - 1) <-  {value = 0; father = -1; depth = 0};
    if debug then begin
      print_string "Assignment: ";
      Array.iter (fun x -> print_int x.value; print_char ' ') assigArray;
      print_string "\n\n" end

  let propag x = stack.(!dpth) <- (x :: stack.(!dpth))

  let restore i = 
    if debug then begin
      print_string "Restore: ";
      for k = 0 to i do (fun l -> print_list l) stack.(k) done;
      print_newline() end;
    for k = i to !dpth do
      List.iter reset stack.(k);
      stack.(k) <- [] done

  let read x = assigArray.((abs x) - 1).value

  let write ?(father = -1) x = assigArray.((abs x) - 1).value <- x;
    assigArray.((abs x) - 1).depth <- !dpth;
    assigArray.((abs x) - 1).father <- father;
    propag x;
    if debug then begin
      print_string "Assignment: ";
      Array.iter (fun x -> print_int x.value; print_char ' ') assigArray;
      print_string "\n\n" end

  let father x = assigArray.((abs x) - 1).father

  let depth x = assigArray.((abs x) - 1).depth


  (* ********************************************************* *)
  (*          Référencement des clauses du problème            *)
  (* ********************************************************* *)

  module Cls = Set.Make
    (struct
      type t = int
      let compare x y = compare (abs x) (abs y)
     end)
  (* Les clauses sont des ensembles d'entiers (+x pour le litéral vrai
     de la variable x, -x pour sa négation), avec la relation de
     comparaison sur les valeurs absolues (entre nom de variable). *)

  let clauseArray = Array.make cls Cls.empty
  (* On référencie l'ensemble des clauses dans un tableau, afin de
     stocker des indices dans nos structures de données plutôt que des
     clauses. *)

  let compt = ref (-1)
  (* L'indice en cours dans le tableau. *)

  let add_clause c = incr compt; clauseArray.(!compt) <- c; !compt

  let fill l =
    incr compt;
    clauseArray.(!compt) <- fold (fun x s -> Cls.add x s) l Cls.empty;
    !compt
  (* Renvoie dans la case du tableau en cours la clause représentée par sa liste d'entiers l. *)

  let clause id = clauseArray.(id)

  let cls_make id = id

  let cls_fold f cls = Cls.fold f (clause cls)

  let length cls = cls_fold (fun x t -> if read x = 0 then t+1 else t) cls 0

  let is_singleton cls = 
    try (
      match Cls.fold 
	(fun x v -> match (read x, v) with
          |(0, 0) -> x
          |(0, _) -> failwith "is_not"
	  |(_, y) ->  y) (clause cls) 0
      with
	|0 -> if debug then begin
	  print_string "Clause insatisfiable: ";
	  print_list (Cls.elements (clause cls));
	  print_newline() end;
	  raise Unsatisfiable
	|x -> x
    )
    with Failure "is_not" -> 0
  (* Renvoie l'unique élément de la clause d'indice id qui n'est pas
     encore assigné quand il est bien unique, 0 sinon.  Lève
     l'exception Unsatisfiable si la clause n'est pas satisfiable.*)

  let literals cls = Cls.elements (clause cls)
  (* Donne les elements d'une clause *)
      
  let choose cls = Cls.choose (clause cls)


  (* ************************************************* *)
  (*        Gestion intelligente du backtrack          *)
  (* ************************************************* *)

  let backtrack x =
    let add = Cls.fold (fun x r -> if depth x = !dpth then x :: r else r) in
    let rec aux c = function
      |[] -> c
      |x :: l -> let c1 = clause (father x) in
		 let l1 = add c1 l in
		 aux (Cls.union c1 (Cls.remove x c)) l1 in
    let c = clause (father x) in
    let c1 = aux c (add c []) in
    let cls = add_clause c1 in
    let d = Cls.fold (fun x d -> max (depth x) d) c1 0 in
    (cls, d)
    
end;;

module type Abstract =
sig
  exception Satisfiable
  exception Unsatisfiable
  type cls = int
  val var : int
  val cls : int
  val lst : int list list
  val ord : (int * int) list
  val wlit : bool
  val heur : string
  val fix_depth : int -> unit
  val restore : int -> unit
  val read : int -> int
  val write : ?father:int -> int -> unit
(*  val father : int -> int
  val depth : int -> int
*)  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val fill : int list -> cls
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val literals : cls -> int list
  val length : cls -> int
  val choose : cls -> int
  val is_singleton : cls -> int
  val backtrack : int -> (cls * int)

end;;

module Make = (Core : Abstract);;


(* Tests *)

(*
Make.var;;
Make.cls;;
Make.lst;;
Make.wlit;;
Make.heur;;
Make.hd Make.ord;;
Make.read 1;;
Make.write 3 ;;
Make.read 3;;
Make.update 0 0 Make.ord;;
*)

(* Anciennes versions *)


(* Assig *)
(* Gere l'assignation des variables *)

(*
module type AssigElt =
  sig
    type t
    val zero : t
    val nbr : int
  end;;
(* nbr est le nombre de variables dans l'instance de SAT, indiquée en début de problème. *)

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

(* Determine l'ordre dans lequel on considere les variables.
La fonction d'ordre en elle meme n'est pas encore implementee  *)

module OrderCore =

  struct
    type order = (int * int) list
    let hd l = snd (List.hd l)
    let tl = List.tl

    let init l = List.map (fun x -> x, x) l
  end;;


module type OrderAbstract =
  sig
    type order
    val hd : order -> int

    val tl : order -> order
    val init : int list -> order
  end;;


module Order = (OrderCore: OrderAbstract);;
*)



(*
module Core = Make
  (struct
    let var = 10
    let cls = 10
    let lst = []
  end);;
*)
