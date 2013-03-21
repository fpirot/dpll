module type CoreElt =
sig
  exception Satisfiable
  val read : int -> int
  val var : int
  val heur : string
  val ord : (int * int) list
end;;

module type Heur =
sig
  type order
  val create : unit -> order
  val extract : order -> int * order
  val is_empty : order -> bool
end;;

module type Clause =
sig
  type cls
  type map
  val length : cls -> int
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val find : int -> map -> cls list
end;;  

module OrderCore = functor (Cor: CoreElt) -> functor (Clause: Clause) ->
struct
  module Rand = Rand.Make (Cor)
  module Default = Default.Make (Cor)
  module Moms = Moms.Make (Clause) (Cor)
  module Dlis = Dlis.Make (Clause) (Cor)
    
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


module type OrderAbstract = functor (Cor : CoreElt) -> functor (Clause: Clause) ->
  sig 
    type order
    val is_empty : order -> bool
    val create : unit -> order
    val extract : Clause.map -> order -> int * order
    val update : int -> Clause.map -> order -> order
  end;;

module Make = (OrderCore : OrderAbstract);;
