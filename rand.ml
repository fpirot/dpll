module type CoreElt =
  sig
    exception Satisfiable
    val heur : string
    val ord : (int * int) list
    val read : int -> int
  end;;

module RandCore = functor (Cor : CoreElt) ->
  struct
    
    type order = int list
    
    let create () = List.map (fun x -> snd x) Cor.ord

    let filter = List.filter (fun x -> Cor.read x = 0)

    let extract lst =
      let l = filter lst in
      let rec modif n = function
        |[] -> failwith "Heur.random"
        |a :: l -> if n = 0 then (a, l)
          else let (b, l) = modif (n - 1) l in (b, a::l) in
      if l = [] then raise Cor.Satisfiable
      else modif (Random.int (List.length l)) l

    let is_empty l = l = []
    
  end;;

module type RandAbstract = functor (Cor : CoreElt) ->
  sig
    type order
    val create : unit -> order
    val extract : order -> int * order
    val is_empty : order -> bool
  end;;

module Make = (RandCore : RandAbstract);;
