module type CoreElt =
  sig
    val heur : string
    val ord : (int * int) list
    val read : int -> int
  end;;

module RandCore = functor (Cor : CoreElt) ->
  struct
    
    type order = int list
    
    let create () = List.map (fun x -> snd x) Cor.ord
(*
    let filter = List.filter (fun x -> Cor.read x = 0)
*)
    let extract lst =
      let rec modif n = function
        |[] -> failwith "Heur.random"
        |a :: l -> if n = 0 then a else modif (n - 1) l in
      if lst = [] then raise Not_found
      else modif (Random.int (List.length lst)) lst

    let rec update x = function
      | [] -> []
      | a :: l -> if abs x = a then l else a :: (update x l)

    let is_empty l = l = []
    
  end;;

module type RandAbstract = functor (Cor : CoreElt) ->
  sig
    type order
    val create : unit -> order
    val extract : order -> int
    val update : int -> order -> order
    val is_empty : order -> bool
  end;;

module Make = (RandCore : RandAbstract);;
