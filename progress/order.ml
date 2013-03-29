module type Core =
sig
  exception Satisfiable
  type cls = int
  val cls : int
  val ord : (int * int) list
  val heur : string
  val read : int -> int
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
end;;

module type Clause =
sig
  type cls
  type map
  val find : int -> map -> cls list
end;;


module OrderCore = functor (Cor: Core) -> functor (Elt: Clause with type cls = Cor.cls) ->
struct

  type map = Elt.map

  module Rand = Rand.Make (Cor)
  module Default = Default.Make (Cor)
  module Moms = Moms.Make (Cor) (Elt)
  module Dlis = Dlis.Make (Cor) (Elt)
    
  type order = Default of Default.order | Rand of Rand.order | Moms of Moms.order | Dlis of Dlis.order
      
  let create () = match Cor.heur with
    | "Rand" -> Rand (Rand.create ())
    | "Moms" -> Moms (Moms.create ())
    | "Dlis" -> Dlis (Dlis.create ())
    | _ -> Default (Default.create ())

  let extract map = function
    | Default (ord) -> let (x,order) = Default.extract ord in (x, Default (order))
    | Rand (ord) -> let (x,order) = Rand.extract ord in (x, Rand (order))
    | Moms (ord) -> let (x,order) = Moms.extract map ord in (x, Moms (order))
    | Dlis (ord) -> let (x,order) = Dlis.extract map ord in (x, Dlis (order))
							 
  let is_empty = function
    | Default (ord) -> Default.is_empty ord
    | Rand (ord) -> Rand.is_empty ord
    | Moms (ord) -> Moms.is_empty ord
    | Dlis (ord) -> Dlis.is_empty ord

  let update x map = function
    | Default (ord) -> Default (ord)
    | Rand (ord) -> Rand (ord)
    | Moms (ord) -> Moms (Moms.update x map ord)
    | Dlis (ord) -> Dlis (Dlis.update x map ord)
end;;


module type OrderAbstract = functor (Cor : Core) -> functor (Elt: Clause with type cls = Cor.cls) ->
sig 
  type order
  type map = Elt.map
  val is_empty : order -> bool
  val create : unit -> order
  val extract : map -> order -> int * order
  val update : int -> map -> order -> order
end;;

module Make = (OrderCore : OrderAbstract);;
