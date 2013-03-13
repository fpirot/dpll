(* On recherche parmi une liste de variables non encore affectées
   celle qui permet de résoudre le plus de clauses parmi celles de
   poids minimum, à l'instant considéré dans le problème. *)

module type Core =
sig
  val length_max : int
  val var : int
end;;

module type Clause =
sig
  type cls
  type elt = int
  val length : cls -> int
  val iter : (elt -> unit) -> cls -> unit
end


module Moms = functor (Core: Core) -> functor (Elt: Clause) ->
struct
  let poids = Array.make_matrix (Core.length_max + 1) (Core.var + 1) 0
  (* poids.(p).(0) contiendra le nombre de clauses de poids p.
     poids.(0) et poids.(1) ne seront jamais considérés, tant pis. *)

  let remove = List.iter (fun c -> let n = Elt.length c in
				   poids.(n).(0) <- poids.(n).(0) - 1;
				   Elt.iter (fun x -> poids.(n).(abs x) <- poids.(n).(abs x) - 1) c)

  let find_size_min () = let i = ref 0 in
			 while (!i < Core.length_max && poids.(!i).(0) = 0) do incr i done;
			 !i

  let add = List.iter (fun c -> let n = Elt.length c in
				poids.(n).(0) <- poids.(n).(0) + 1;
				Elt.iter (fun x -> poids.(n).(abs x) <- poids.(n).(abs x) + 1) c)

  let find_xmoms l = 
    let n = find_size_min () in
    let xmoms = ref 0 and v = ref 0 in
    List.iter (fun x -> if poids.(n).(x) > !v then (v := poids.(n).(x); xmoms := x)) l;
    !xmoms
  (* Choisit parmi une liste de variables celle qui correspond au choix de MoMS. *)

end;;
