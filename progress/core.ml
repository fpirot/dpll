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

  let debug = true

  let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  type cls = int

  let depth = ref 0

  let fix_depth i = depth := i

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
    

  (* ********************************************************* *)
  (*        Gestion des affectations et des backtracks         *)
  (* ********************************************************* *)
      
  let stack = Array.make var []
(* La "pile" contenant les assignations successives : on utilise un 
   tableau de liste en partant du principe qu'il y a au plus n étages
   de paris, avec n le nombre de variables. *)

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

  let propag x = stack.(!depth) <- (x :: stack.(!depth))

  let restore i = 
    if debug then begin
      print_string "Restore: ";
      Array.iter (fun l -> print_list l) stack;
      print_newline()
    end;
    for k = i to !depth do
      List.iter reset stack.(k);
      stack.(k) <- [] done

  let read x = assigArray.((abs x) - 1).value

  let write ?(father = -1) x = assigArray.((abs x) - 1).value <- x;
    assigArray.((abs x) - 1).depth <- !depth;
    assigArray.((abs x) - 1).father <- father;
    propag x;
    if debug then begin
      print_string "Assignment: ";
      Array.iter (fun x -> print_int x.value; print_char ' ') assigArray;
      print_string "\n\n" end

  let father x = assigArray.((abs x) - 1).father

  let depth x = assigArray.((abs x) - 1).depth

  let fold = List.fold_right
    
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
  val father : int -> int
  val depth : int -> int
  val reset : int -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
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
