module type CoreElt =
sig
	type cls
	val graph : bool
	val var : int
	val read : int -> int
  val iter : (int -> unit) -> cls -> unit
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val literals : cls -> int list
  val father : int -> cls
  val depth : int -> int
end;;

module Print = functor (Cor : CoreElt) ->
struct
	
	let fresh =
		let compt = ref 0 in
			fun () -> incr compt; !compt

	let colors channel arr =
		Array.iteri (fun i x -> if x then let v = abs_float (4. *. float (Cor.depth (i+1))) /. (float Cor.var) in
			Printf.fprintf channel "\n%d[style=filled,color=\034%f %f %f\034];" (Cor.read (i+1)) v v v) arr 
	
	let rec others channel = function
		|[] -> ()
		|(x, y):: l -> Printf.fprintf channel "\n%d -> %d" x y; others channel l

	let rec currents channel = function
		|[] -> ()
		|(x, y):: l -> Printf.fprintf channel "\n%d -> %d" x y; currents channel l

	let file arr (lcur, loth) =
		let channel = open_out ("Graph/graph"^(string_of_int (fresh ()))^".dot") in
			Printf.fprintf channel "digraph G {\nsize =\034%d, %d\034;" Cor.var Cor.var;
			colors channel arr;
			Printf.fprintf channel "\nsubgraph{";
			currents channel lcur;
			Printf.fprintf channel "\n}";
			others channel loth;
			Printf.fprintf channel "\n}";
			close_out channel

end;;

module GraphCore = functor (Cor : CoreElt) ->
struct

	module Print = Print(Cor)

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


	let draw cls =

	let arr = Array.make Cor.var false
	and partition a l lst =
		let c = Cor.father a
		and max = ref min_int in
			Cor.iter (fun x -> if (Cor.depth x) > !max then max := (Cor.depth x)) c;
			(Cor.cls_fold (fun x ((l1, l2), l3) -> if (Cor.depth x) = !max then
				(((x, a)::l1, l2), x::l3) else ((l1, (x, a)::l2), l3)) c (lst, l)) in

		let rec iterate lst = function
			|[] -> lst
			|a::l -> arr.(abs a - 1) <- true;
			let (lst', l') = partition a l lst in
				iterate lst' l'

	in
		let lst = iterate ([],[]) (Cor.literals cls) in
			Print.file arr lst
		
		
end;;

module type GraphAbstract = functor (Cor : CoreElt) ->
sig
	type cls = Cor.cls
	val draw : cls  -> unit 
	(*val convert : int list -> cls*)
end;;

module Make = (GraphCore : GraphAbstract);;
