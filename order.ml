module type CoreElt =
  sig
    val heur : string
    val ord : (int * int) list
  end;;

module type HeurElt =
  sig
    val random : 'a list -> 'a list
  end;;

module OrderCore = functor (Cor : CoreElt) -> functor (Heur : HeurElt) ->
  struct
    
    let debug = false
    
    type order = (int * int) list
    
    let create () = Cor.ord
    
    let hd l = snd (List.hd l)
    
    let tl l =if debug then begin
        print_string "Order: ";
        List.iter (fun x -> print_int (snd x); print_char ' ') (List.tl l);
        print_string "\n\n" end; List.tl l
        
    let update x y l = 
      if debug then begin
        print_string "Order: ";
        List.iter (fun x -> print_int (snd x); print_char ' ') l;
        print_string "\n\n" end;
      if Cor.heur = "Rand" then Heur.random l else l
        
    let is_empty l = l = []
    
    let fold = List.fold_right
  
  end;;

module type OrderAbstract = functor (Cor : CoreElt) -> functor (Heur : HeurElt) ->
  sig 
    type order
    val create : unit -> order
    val hd : order -> int
    val tl : order -> order
    val update : int -> int -> order -> order
    val is_empty : order -> bool
    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  end;;

module Make = (OrderCore : OrderAbstract);;
