module type Core =
sig
  type cls = int
  val nb_cls : unit -> int
  val ord : (int * int) list
  val heur : string
  val read : int -> int
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;


module OrderCore = functor (Cor: Core) ->
struct

  type cls = Cor.cls

  module Rand = Rand.Make (Cor)
  module Default = Default.Make (Cor)
  module Moms = Moms.Make (Cor)
  module Dlis = Dlis.Make (Cor)
    
  type order = Default of Default.order | Rand of Rand.order | Moms of Moms.order | Dlis of Dlis.order
      
  let create () = match Cor.heur with
    | "Rand" -> Rand (Rand.create ())
    | "Moms" -> Moms (Moms.create ())
    | "Dlis" -> Dlis (Dlis.create ())
    | _ -> Default (Default.create ())

  let extract = function
    | Default (ord) -> Default.extract ord
    | Rand (ord) -> Rand.extract ord
    | Moms (ord) -> Moms.extract ord
    | Dlis (ord) -> Dlis.extract ord 
	
  let add x = function
    | Default (ord) -> Default (ord)
    | Rand (ord) -> Rand (ord)
    | Moms (ord) -> Moms (Moms.add [x] ord)
    | Dlis (ord) -> Dlis (Dlis.add [x] ord)
						 
  let is_empty = function
    | Default (ord) -> Default.is_empty ord
    | Rand (ord) -> Rand.is_empty ord
    | Moms (ord) -> Moms.is_empty ord
    | Dlis (ord) -> Dlis.is_empty ord

  let update x lst = function
    | Default (ord) -> Default (Default.update x ord)
    | Rand (ord) -> Rand (Rand.update x ord)
    | Moms (ord) -> Moms (Moms.update x lst ord)
    | Dlis (ord) -> Dlis (Dlis.update x lst ord)
end;;


module type OrderAbstract = functor (Cor : Core) ->
sig 
  type order
  type cls = Cor.cls
  val is_empty : order -> bool
  val create : unit -> order
  val add : cls -> order -> order
  val extract : order -> int
  val update : int -> (cls list * cls list) -> order -> order
end;;

module Make = (OrderCore : OrderAbstract);;
