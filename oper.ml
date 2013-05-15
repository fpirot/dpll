(* Module d'operation sur les clauses propres a DPLL *)

module type CoreElt =
sig
  type cls = int
  type proof
  exception Satisfiable
  exception Unsatisfiable of cls
  val nb_cls : unit -> int
  val var : int
  val wlit : bool
  val graph : bool
  val lst : int list list
  val read : int -> int
  val write : ?father: cls -> int -> unit
  val write_father : cls -> int -> unit
  val father : int -> cls
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val heur : string
  val ord : (int * int) list
  val is_singleton : cls -> int
  val choose : cls -> int
  val literals : cls -> int list
  val cls_make : int -> cls
  val length : cls -> int
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val backtrack : cls -> bool -> int
  val proof : cls -> proof
end;;

module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
sig
  type cls
  type map
  val empty : map
  val create : int list list -> map
  val find : int -> map -> cls list
  val bindings : map -> (int * int list list) list
  val extract : int -> map -> map
  val add : cls -> map -> map
  val is_empty : map -> bool
end;;

module type Order =
sig
  type order
  type map
  type cls
  val is_empty : order -> bool
  val create : unit -> order
  val add : cls -> order -> order
  val extract : map -> order -> int
  val update : int -> map -> order -> order
end;;

module type WlitElt =
sig
  type cls
  type set
  type wlit
  val wempty : wlit
  val update : int -> wlit -> wlit * set * set
  val update_cls : cls -> wlit ->  wlit
  val fold : (cls -> 'a -> 'a) -> set -> 'a -> 'a
  val choose : set -> int
  val singleton : int -> set
  val empty : set
  val is_empty : set -> bool
  val union : set -> set -> set
  val add : int -> set -> set
  val remove : int -> set -> set
  val elements : set -> int list
end;;

module type Graph =
sig
  type cls
  val draw : cls -> unit
end;;

module type Proof =
sig
  type proof
  val file : proof -> unit
end;;

module OpCore = functor (Cor: CoreElt) -> functor (Elt: OpElt with type cls = Cor.cls) 
    -> functor (Wlit: WlitElt with type cls = Cor.cls)
        -> functor (Ord: Order with type map = Elt.map and type cls = Cor.cls)
	    -> functor (Graph: Graph with type cls = Cor.cls)
		-> functor (Proof: Proof with type proof = Cor.proof) ->
struct

  type env = {clause: Elt.map; order: Ord.order; wlit: Wlit.wlit}
  type cls = Cor.cls
  type set = Wlit.set

  exception Backtrack of int

  let nxt_print = ref (if Cor.graph then 1 else -1)

  let debug = false
  let print_list l =
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  let create () = let m = Elt.create Cor.lst in
		  let ord = Ord.create() in
		  let w = if Cor.wlit then Wlit.update_cls 0 Wlit.wempty else Wlit.wempty in
		  {clause = m; order = ord; wlit = w}

  (* Extrait une variable selon l'ordre *)
  let extract env = Ord.extract env.clause env.order

  let update n env = 
    let p = Cor.nb_cls () in
    let w = if Cor.wlit then Wlit.update_cls n env.wlit else Wlit.wempty
    and map = ref env.clause and ord = ref env.order in
    for i = n to p-1 do
      map := Elt.add i !map; ord := Ord.add i !ord done;
{clause = !map; order = !ord; wlit = w}

  let remove x env = {clause = Elt.extract x env.clause; order = Ord.update x env.clause env.order; wlit = env.wlit}
   
  let is_empty env = Elt.is_empty env.clause


  (* ***************************************************** *)
  (*       Gestion de la propagation des contraintes       *)
  (* ***************************************************** *)

  let prop l = List.fold_right (fun c s -> let x = Cor.is_singleton c in
					   if x <> 0 then (
					     if debug then begin
					       print_string "Select: ";
					       print_int x;
					       print_string ", because of ";
					       print_list (Cor.literals c);
					       print_newline()
					     end;
					     Cor.write_father x c;
					     Wlit.add x s)
					   else s) l Wlit.empty
  (* Sélectionne dans une liste de clauses celles qui sont des
     singletons, et renvoie l'union de leurs éléments. On renvoie
     ainsi un ensemble de nouvelles assignations contraintes par celle
     en cours. *)

  let find x env = Elt.find (-x) env.clause

  let entail x env = prop (find x env)

  let simple_propagation l env =
    let rec aux env setv =
      if Wlit.is_empty setv then env
      (* Lorsqu'on n'a plus d'assignations contraintes, la propagation
	 s'arrête. On rentre la liste des assignations effectuée au cours de
	 cette propagation dans une liste, et on passe au prochain pari. *)
      else begin
	let x = Wlit.choose setv in
	if debug then begin
	  print_string "Choice: ";
	  print_int x;
	  print_newline()
	end;
	Cor.write x;
	let sbord = entail x env in
	let setv' = Wlit.union sbord setv in
	aux (remove x env) (Wlit.remove x setv')
      end in
    let setv = prop l in
    aux env setv

  let wlit_propagation l env =
    let rec aux env setv = 
      if Wlit.is_empty setv then env
      (* Lorsqu'on n'a plus d'assignations contraintes, la propagation
	 s'arrête. On rentre la liste des assignations effectuée au cours de
	 cette propagation dans une liste, et on passe au prochain pari. *)
      else begin
	let x = Wlit.choose setv in
	if debug then begin
          print_string "WLit choose: ";
          print_int x;
          print_newline()
        end;
	Cor.write x;
	let (w, sbord, ssat) = Wlit.update x env.wlit in
	let setv' = Wlit.union sbord setv in
	aux (remove x env) (Wlit.remove x setv')
      end in
    let setv = prop l in
    aux env setv

  let print_conflict c = 
    let rec aux () =
      print_string "Que voulez-vous faire maintenant ?
  g : engendrer un graphe des conflits
  r : afficher la preuve par resolution de l'engendrement de cette clause
  c : continuer jusqu'au prochain conflit
  s n : continuer jusqu'au n-ieme prochain conflit
  t : terminer l'execution sans s'arreter\n";
      match read_line() with
	|"g" -> Graph.draw c; aux()
	|"r" -> Proof.file (Cor.proof c); aux()
	|"c" -> nxt_print := 1
	|"t" -> nxt_print := -1
	| s -> try (match String.sub s 0 2 with
	    | "s " -> (try nxt_print := int_of_string (String.sub s 2 (String.length s - 2)) with _ -> aux())
	    | _ -> aux())
	  with _ -> aux() in
    decr nxt_print;
    let k = Cor.backtrack c (!nxt_print = 0) in
    if !nxt_print = 0 then aux (); k

  let propagation =
    let propag = if Cor.wlit then wlit_propagation
      else simple_propagation in
    (fun l env channel ->
      try propag l env with Cor.Unsatisfiable c ->
	let k = print_conflict c in
      	raise (Backtrack k))

  let bindings env = Elt.bindings env.clause

end;;


module type OpAbstract = functor (Cor : CoreElt) -> functor (Elt : OpElt with type cls = Cor.cls) 
    -> functor (Wlit: WlitElt with type cls = Cor.cls) 
        -> functor (Ord: Order with type map = Elt.map and type cls = Cor.cls) 
	    -> functor (Graph: Graph with type cls = Cor.cls)
		-> functor (Proof: Proof with type proof = Cor.proof) ->
sig
  type env
  type cls = int
  type set
  exception Backtrack of int
  val create : unit -> env
  val is_empty : env -> bool
  val extract : env -> int
  val remove : int -> env -> env
  val update : int -> env -> env
  val entail : int -> env -> set
  val propagation : cls list -> env -> out_channel -> env
  val bindings : env -> (int * int list list) list
  val find : int -> env -> cls list
end;;

module Make = (OpCore : OpAbstract);;
