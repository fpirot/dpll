module HeurCore =
  struct
    
    let random lst =
      let rec modif n = function
        |[] -> failwith "Heur.random"
        |a :: l -> if n = 0 then (a, l)
          else let (b, l) = modif (n - 1) l in (b, a::l) in
      if lst = [] then []
      else let (a, l) = modif (Random.int (List.length lst)) lst in a::l
    
  end;;

module type HeurAbstract =
  sig
    val random : 'a list -> 'a list
  end;;

module Make = (HeurCore : HeurAbstract);;
