(* Module de, surprise, chargement des donnees *)

module Load =
struct

  exception Useless

  let edge channel graph = Scanf.bscanf channel " %d %d"
    (fun u v -> graph.(u) <- v::(graph.(u));	graph.(v) <- u::(graph.(v)))

  let rec read channel graph = function
    |0 -> graph
    |n ->	Scanf.bscanf channel "%c"
      (fun c -> match c with
	|'e' -> edge channel graph; read channel graph (n-1)
	|_ -> Scanf.bscanf channel "%[^\n]\n" (fun _ -> ()); read channel graph n)

  let init channel n e =
    let graph = Array.make n [] in
    read channel graph e
      
  let load channel =
    Scanf.bscanf channel "p edge %d %d" (fun n e -> n, init channel n e)

end;;

Load.load (Scanf.Scanning.open_in "test");;
