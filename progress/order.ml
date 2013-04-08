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
(*      
  let hd = function
    | Default (ord) -> Default.hd ord
    | Rand (ord) -> Rand.hd ord
      
  let tl = function
    | Default (ord) -> Default (Default.tl ord)
    | Rand (ord) -> Rand (Rand.tl ord)
      
  let update x y = function
    | Default (ord) -> Default (Default.update x y ord)
    | Rand (ord) -> Rand (Rand.update x y ord)
*)
  let extract map = function
    | Default (ord) -> Default.extract ord
    | Rand (ord) -> Rand.extract ord
    | Moms (ord) -> Moms.extract map ord
    | Dlis (ord) -> Dlis.extract map ord
      
  let is_empty = function
    | Default (ord) -> Default.is_empty ord
    | Rand (ord) -> Rand.is_empty ord
    | Moms (ord) -> Moms.is_empty ord
    | Dlis (ord) -> Dlis.is_empty ord

  let update x map = function
    | Default (ord) -> Default (Default.update x ord)
    | Rand (ord) -> Rand (Rand.update x ord)
    | Moms (ord) -> Moms (Moms.update x map ord)
    | Dlis (ord) -> Dlis (Dlis.update x map ord)
end;;


module type OrderAbstract = functor (Cor : Core) -> functor (Elt: Clause with type cls = Cor.cls) ->
  sig 
    type order
    type map = Elt.map
    val is_empty : order -> bool
    val create : unit -> order
    val extract : map -> order -> int
    val update : int -> map -> order -> order
  end;;

module Make = (OrderCore : OrderAbstract);;
