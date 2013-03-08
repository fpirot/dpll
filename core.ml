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


  let init str n channel =
    let rec next () = match Scanf.bscanf channel " %d " (fun x -> x) with
      | 0 -> ()
      | n -> next ()
	
    and iter s c lst lstC ensV = function
      | 0 -> let s = try Scanf.bscanf channel "c %s@\n" (fun x -> s ^ x ^ "\n") with _ -> s in
	     Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then (c + 1, ensV :: lstC, lst, s)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) 0 with
		       Useless -> (c, lstC, lst, s) end)
      | n -> let s = try Scanf.bscanf channel "c %s@\n" (fun x -> s ^ x ^ "\n") with _ -> s in
	     Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then
		   iter s c lst (ensV :: lstC) [] (n - 1)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) n with
		       Useless ->
			 (next (); iter s (c - 1) lst lstC [] (n - 1)) end) in
    
    iter str n [] [] [] n;;


  let load channel =
    let s = try Scanf.bscanf channel "c %s@\n" (fun x -> x ^ "\n") with _ -> "" in
    Scanf.bscanf channel "p cnf %d %d" (fun v c -> (v, init s (c-1) channel))

end;;


(* Regroupe les modules d'initialisation *)

module Core =
  struct
    
    type order = (int * int) list
    
    let debug = false
    
    let (wlit, heur, path) =
      let w = ref false
      and s = ref "Nil"
      and p = ref "ex0.cnf" in
        Arg.parse [("-wlit", Arg.Unit(fun () -> w := true), "Watched literals");
        	("-rand", Arg.Unit(fun () -> s := "Rand"), "Random selection");
        	("-moms", Arg.Unit(fun () -> s := "Moms"), "Maximum Occurrences in clauses of Minimum Size");
        	("-dlis", Arg.Unit(fun () -> s := "Dlis"), "Dynamic Largest Individual Sum")]
            (fun str -> p := str) "";
        (!w, !s, !p)
        
    let (var, (cls, lst, ord, comment)) = Load.load (Scanf.Scanning.open_in ("Test/"^path))
    
    
    let assigArray = Array.create var 0
    
    let read x = assigArray.((abs x) - 1)
    
    let write x = assigArray.((abs x) - 1) <- x;
      if debug then begin
        print_string "Assignment: ";
        Array.iter (fun x -> print_int x; print_char ' ') assigArray;
        print_string "\n\n" end
        
    let reset x = assigArray.((abs x) - 1) <- 0;
      if debug then begin
        print_string "Assignment: ";
        Array.iter (fun x -> print_int x; print_char ' ') assigArray;
        print_string "\n\n" end
    
    
    let hd l = snd (List.hd l)
    
    let tl l =if debug then begin
        print_string "Order: ";
        List.iter (fun x -> print_int (snd x); print_char ' ') (List.tl l);
        print_string "\n\n" end; List.tl l
        
    let update x y l = 
      if debug then begin
        print_string "Order: ";
        List.iter (fun x -> print_int (snd x); print_char ' ') l;
        print_string "\n\n" end; l
        
    let is_empty l = l = []
    
    let fold = List.fold_right
    
    
  end;;

module type Abstract =
  sig
    type order
    val var : int
    val cls : int
    val lst : int list list
    val ord : order
    val wlit : bool
    val heur : string
    val read : int -> int
    val write : int -> unit
    val reset : int -> unit
    val hd : order -> int
    val tl : order -> order
    val update : int -> int -> order -> order
    val is_empty : order -> bool
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