module type Core =
sig
  type cls = int
  exception Satisfiable
  exception Unsatisfiable of cls
  val var : int
  val nb_cls : unit -> int
  val read : int -> int
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val literals : cls -> int list
end;;

module WlitCore =  functor (Cor: Core) -> 
struct

  type cls = Cor.cls

  module St = Set.Make (
    struct
      type t = int
      let compare = compare
    end)

  type set = St.t

  module Watched = Map.Make (struct
    type t = cls
    let compare = compare
  end)

  type watched = (int * int) Watched.t
  (* Associe à chaque clause son couple de variables surveillées. *)

  module Assoc = Map.Make (struct
    type t = int
    let compare x y = compare (abs x) (abs y)
  end)

  type assoc = St.t Assoc.t
(* Associe à chaque littéral l'ensemble des clauses dans lesquelles il
   est surveillé. *)

  type wlit = watched * assoc

  let assoc x w = Assoc.find x (fst w)

  let watched c w = Watched.find c (snd w)

  let get_sat x c w = let (a,b) = watched c w in
		      x = a || x = b

  exception Finished of int * int
  let make_watched c sbord ssat =
    try (
      match List.fold (fun x -> 
	match Cor.read x with
	  | x -> raise Core.Satisfiable
	  | 0 -> a = 0 then (a,0) else raise (Finished (a,x))
	  | _ -> (a,b)) (Cor.elements c)
      with
	| (0,0) -> raise Core.Unsatisfiable c
	| (a,0) -> ((a,0), St.add x sbord, ssat)
	| _ -> failwith "Match error: wlit"
    with
      | Satisfiable -> ((0,0), sbord, St.add c ssat)
      | Finished x -> (x, sbord, ssat)

  let add_cls c wlit = 
    let sbord = ref St.empty and ssat = ref St.empty in
    let (a,b) = make_watched c sbord ssat in
    let watched = Watched.add c (a,b) (fst wlit)
    and assoc1 =
      if a <> 0 then Assoc.add a (St.add c (Assoc.find a (snd wlit))) (Assoc.add b (St.add c (Assoc.find b (snd wlit))) snd wlit)
    in (watched, assoc)

  let update_cls n wlit =
    let p = Cor.nb_cls () in
    for i = n to p-1 do add_cls i done

  let update x w =
    let s = assoc x w in
    List.fold_right (fun c (w, sbord, ssat) -> if get_sat x c w then (w, sbord, St.add c lsat) else (new_assoc c sbord ssat
    let sbord = ref St.empty and lsat = ref St.empty in
    
