(* Module de, surprise, chargement des donnees *)

module Load =
		struct

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


		let init c channel =
			let rec iter list lstC lstV = function
				  | 0 -> Scanf.bscanf channel " %d "
				      (fun x -> if x = 0 then (lstV :: lstC, list)
				              else iter (insert (abs x) list) lstC (x :: lstV) 0)
				  | n -> Scanf.bscanf channel " %d "
				      (fun x -> if x = 0 then iter list (lstV :: lstC) [] (n - 1)
				              else iter (insert (abs x) list) lstC (x :: lstV) n) in
				iter [] [] [] (c - 1)


		let load channel =
			Scanf.bscanf channel "p cnf %d %d" (fun v c -> (v, c, init c channel))

  end;;


(* Regroupe les modules d'initialisation *)

module Core =
  struct
    
    type order = (int * int) list
    
    let debug = true
    let (var, cls, (lst, ord)) = Load.load (Scanf.Scanning.open_in "test")
    
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
    let update l = 
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
    val read : int -> int
    val write : int -> unit
    val reset : int -> unit
    val hd : order -> int
    val tl : order -> order
    val update : order -> order
    val is_empty : order -> bool
    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  end;;

module Make = (Core : Abstract);;


(* Tests *)

(*
Make.var;;
Make.cls;;
Make.lst;;
Make.hd Make.ord;;
Make.read 1;;
Make.write 3 2;;
Make.read 3;;
Make.update Make.ord;;
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
