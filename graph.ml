module type CoreElt =
sig
	type cls
	val graph : bool
	val var : int
	val read : int -> int
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val iter : (int -> unit) -> cls -> unit
  val literals : cls -> int list
  val father : int -> cls
  val depth : int -> int
  (*val convert : int list -> cls*)
end;;

module Print =
struct
	
	let fresh =
		let compt = ref 0 in
			fun () -> incr compt; !compt
	
	let others channel lst f_read = ()

	let currents channel lst f_read = ()

	let file (l_int, l_ext) =
		let channel = open_out ("Graph/graph"^(string_of_int (fresh ()))^".dot") in
			
			close_out channel

end;;

module GraphCore = functor (Cor : CoreElt) ->
struct

	let debug = true
  let print_list l=
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l
	let print_intlist a =
		print_string "(";
		print_int (fst a);
		print_string ", ";
		print_list (Cor.literals (snd a));
		print_string ")"
	let print_some a = match a with
		|None -> print_string "None"
		|Some(a,b) ->
			print_string "(";
			print_int a;
			print_string ", ";
			print_int b;
			print_string ")"


	type cls = Cor.cls


	let find cls chan =

	let arr = Array.make Cor.var false
	and partition a l lst =
		let c = Cor.father a
		and max = ref min_int in
			(Cor.cls_fold (fun x (l1, l2) -> if (Cor.depth x) = !max then ((x, a)::l1, l2) else (l1, (x, a)::l2)) c lst,
				Cor.cls_fold (fun x l -> if (Cor.depth x) > !max then max := (Cor.depth x); x::l) c l) in

		let rec iterate lst = function
			|[] -> lst
			|a::l -> arr.(abs a - 1) <- true;
			let (lst', l') = partition a l lst in
				iterate lst' l'

	in
		let lst = iterate ([],[]) (Cor.literals cls) in
			Print.file lst
		
		
end;;

module type GraphAbstract = functor (Cor : CoreElt) ->
sig
	type cls = Cor.cls
	val find : cls -> out_channel -> unit 
	(*val convert : int list -> cls*)
end;;

module Make = (GraphCore : GraphAbstract);;


(* Test *)

(*
module Graph = Make (struct
		type cls = int list
		let var  = 10
		let graph = true
		let read x = x
		let literals x = x
		let iter = List.iter
		let cls_fold = List.fold_right
		let convert x = x
		let father x = let x = abs x in [x + var; -x - var; x]
	end);;

let g = Graph.create ();;
let g = Graph.add 1 (Graph.convert [2]) g;;
let g = Graph.add 2 (Graph.convert [3; 4]) g;;
let g = Graph.add 3 (Graph.convert [4]) g;;
let g = Graph.add 4 (Graph.convert [5; 6; 7]) g;;
let g = Graph.add 5 (Graph.convert [6; 7]) g;;
let g = Graph.add 6 (Graph.convert [7]) g;;
let g = Graph.add 7 (Graph.convert [8]) g;;
let g = Graph.add 8 (Graph.convert []) g;;
Graph.find 1 8 g;;

let g = Graph.create ();;
let g = Graph.add 1 [2; 7] g;;
let g = Graph.add 2 [7] g;;
let g = Graph.add 3 [4; 7] g;;
let g = Graph.add 4 [7] g;;
let g = Graph.add 5 [6; 7] g;;
let g = Graph.add 6 [7] g;;
let g = Graph.add 7 [] g;;
Graph.find 1 7 g;;
*)
