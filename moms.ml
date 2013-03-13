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
end;;

module type Tas =
sig
  type t
  type elt = int*int
  val empty : t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
  val extract_min : t -> elt * t
end;;


module Moms = functor (Core: Core) -> functor (Elt: Clause) -> functor (Tas: Tas) ->
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

  let load length = Tas.iter (fun x -> poids.(length).(snd x) <- fst x)
  (* Remet le tableau poids à un état précédent grâce à un tas en argument. *)

  let update_tas t = 
    let n = find_size_min () in
    Tas.fold (fun x t -> Tas.add (poids.(n).(snd x),snd x) t) t Tas.empty
(* Actualise le tas qui correspond à l'ordre, selon les nouvelles valeurs de poids. *)

end;;
