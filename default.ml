module type CoreElt =
  sig
    val read : int -> int
    val heur : string
    val ord : (int * int) list
  end;;

module DefaultCore = functor (Cor : CoreElt) ->
  struct

    let debug = false
    
    type order = (int * int) list
    
    let create () = Cor.ord

    let rec extract l =
      try (
	let x = snd (List.hd l) in
	if Cor.read x = 0 then begin
	  if debug then begin
	    print_string "Order: ";
	    print_int x;
	    print_string ", ";
	    print_string "[";
	    List.iter (fun y -> print_int (snd y); print_string " ") (List.tl l);
	    print_string "]\n" end;
	  x end
	else extract (List.tl l))
      with _ -> raise Not_found

    let rec update x = function
      | [] -> []
      | a :: l -> if abs x = snd a then l else a :: (update x l)

    let is_empty l = l = []
  
  end;;

module type DefaultAbstract = functor (Cor : CoreElt) ->
  sig
    type order
    val create : unit -> order
    val extract : order -> int
    val update : int -> order -> order
    val is_empty : order -> bool
  end;;

module Make = (DefaultCore : DefaultAbstract);;
