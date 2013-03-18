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
  type elt = int
  val length : cls -> int
  val clause : int -> cls
  val cls_fold : (elt -> 'a -> 'a) -> cls -> 'a -> 'a
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
  let extract = function
    | Default (ord) -> Default.extract ord
    | Rand (ord) -> Rand.extract ord
    | Moms (ord) -> Moms.extract ord
    | Dlis (ord) -> Dlis.extract ord
      
  let is_empty = function
    | Default (ord) -> Default.is_empty ord
    | Rand (ord) -> Rand.is_empty ord
    | Moms (ord) -> Moms.is_empty ord
    | Dlis (ord) -> Dlis.is_empty ord
      
end;;


module type OrderAbstract = functor (Cor : CoreElt) ->
  sig 
    type order
    val create : unit -> order
    val extract : oder -> int * order
    val is_empty : order -> bool
  end;;

module Make = (OrderCore : OrderAbstract);;
