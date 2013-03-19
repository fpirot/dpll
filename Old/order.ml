module type CoreElt =
  sig
    val heur : string
    val ord : (int * int) list
  end;;

module type Heur =
  sig
    type order
    val create : unit -> order
    val hd : order -> int
    val tl : order -> order
    val update : order -> order
    val is_empty : order -> bool
  end;;

module OrderCore = functor (Cor : CoreElt) ->
  struct
    
    module Rand = Rand.Make (Cor)
    module Default = Default.Make (Cor)
    
    type order = Default of Default.order | Rand of Rand.order
    
    let create () = match Cor.heur with
      | "Rand" -> Rand (Rand.create ())
      | _ -> Default (Default.create ())
    
    let hd = function
      | Default (ord) -> Default.hd ord
      | Rand (ord) -> Rand.hd ord
    
    let tl = function
      | Default (ord) -> Default (Default.tl ord)
      | Rand (ord) -> Rand (Rand.tl ord)
    
    let update x y = function
      | Default (ord) -> Default (Default.update x y ord)
      | Rand (ord) -> Rand (Rand.update x y ord)
    
    let is_empty = function
      | Default (ord) -> Default.is_empty ord
      | Rand (ord) -> Rand.is_empty ord
    
  end;;


module type OrderAbstract = functor (Cor : CoreElt) ->
  sig 
    type order
    val create : unit -> order
    val hd : order -> int
    val tl : order -> order
    val update : int -> int -> order -> order
    val is_empty : order -> bool
  end;;

module Make = (OrderCore : OrderAbstract);;
