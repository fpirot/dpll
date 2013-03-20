module type CoreElt =
  sig
    exception Satisfiable
    val read : int -> int
    val heur : string
    val ord : (int * int) list
  end;;

module DefaultCore = functor (Cor : CoreElt) ->
  struct
    
    let debug = true
    
    type order = (int * int) list
    
    let create () = Cor.ord
(*    
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
*)
    let rec extract = function
      |[] -> raise Not_found
      |x :: l ->
	if Cor.read (snd x) = 0 then 
	  (if debug then begin 
	    print_string "Order: ";
	    print_int (snd x);
	    print_string ", ";
	    print_string "[";
	    List.iter (fun y -> print_int (snd y); print_string " ") l;
	    print_string "]" end;
	   (snd x,l))
	else extract l

    let is_empty l = l = []
  
  end;;

module type DefaultAbstract = functor (Cor : CoreElt) ->
  sig
    type order
    val create : unit -> order
    val extract : order -> int * order
    val is_empty : order -> bool
  end;;

module Make = (DefaultCore : DefaultAbstract);;
