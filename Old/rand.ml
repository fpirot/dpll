module type CoreElt =
  sig
    val heur : string
    val ord : (int * int) list
  end;;

module RandCore = functor (Cor : CoreElt) ->
  struct
    
    type order = int list
    
    let create () = List.map (fun x -> snd x) Cor.ord
    
    let update x y lst =
      let rec modif n = function
        |[] -> failwith "Heur.random"
        |a :: l -> if n = 0 then (a, l)
          else let (b, l) = modif (n - 1) l in (b, a::l) in
      if lst = [] then []
      else let (a, l) = modif (Random.int (List.length lst)) lst in a::l
    
    let hd = List.hd
    
    let tl = List.tl
    
    let is_empty l = l = []
    
  end;;

module type RandAbstract = functor (Cor : CoreElt) ->
  sig
    type order
    val create : unit -> order
    val hd : order -> int
    val tl : order -> order
    val update : int -> int -> order -> order
    val is_empty : order -> bool
  end;;

module Make = (RandCore : RandAbstract);;
