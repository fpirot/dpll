module type CoreElt =
sig
	val graph : bool
	val var : int
	val read : int -> int
end;;

module Print =
struct
	
	let file v g a =
		let channel = open_out "graph.dot" in
		Printf.fprintf channel "digraph G {\nsize =\034%d, %d\034;" v v;
		Array.iteri (fun i x -> match x with
			|None ->()
			|Some(a, b) -> if abs b = i then let v = abs_float (2. *. float b) /. (float v) in
				Printf.fprintf channel "\n%d[shape=box,style=bold,color=\034%f %f %f\034];" b v v v) a;
		List.iter (fun (x, l) ->
			List.iter (fun y ->
				match a.(abs y - 1) with
					|None -> ()
					|Some(a, b) -> let v = abs_float (2. *. float b) /. (float v) in
						Printf.fprintf channel "\nnode [style=filled,color=\034%f %f %f\034];\n%d -> %d;" v v v x y) l) g;
		Printf.fprintf channel "}";
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
		print_list (snd a);
		print_string ")"
	let print_some a = match a with
		|None -> print_string "None"
		|Some(a,b) ->
			print_string "(";
			print_int a;
			print_string ", ";
			print_int b;
			print_string ")"


	module St = Set.Make (struct
			type t = int
			let compare = compare
		end)

	type graph = (int * int list) list

	let create () = []

	let add x l lst =
		if debug then begin
			print_string "List: ";
			print_intlist (x, l);
			print_string " ";
			List.iter (fun l -> print_intlist l) lst;
			print_newline();
		end;
		(x, l)::lst


	let find s t lst =

		let adj = Array.make Cor.var St.empty
		and cnx = Array.make Cor.var None
		and next =
			let compt = ref 0 in
			fun () -> incr compt; !compt in

		let print () =
			print_string "Adj: ";
			Array.iteri (fun i s -> if i > 0 then print_string "; ";
				print_list (St.elements s)) adj;
			print_newline();
			print_string "Cnx: ";
			Array.iteri (fun i c -> if i > 0 then print_string "; ";
				print_some c) cnx;
			print_newline();
			print_newline();
		and receive x y = match cnx.(abs x - 1), cnx.(abs y - 1) with
			|_, None -> ()
			|None, Some(c, d) -> failwith "Graph: receive"
			|Some(a, b), Some(c, d) -> cnx.(abs x - 1) <-
				(if a < c then Some(a, b) else Some(c, d)) in

			List.iter (fun (x, l) -> adj.(abs x - 1) <- List.fold_right
					(fun y s -> adj.(abs y - 1) <- St.add x adj.(abs y - 1); St.add y s)
				l adj.(abs x - 1)) lst;
			(*cnx.(s - 1) <- Some (0, Cor.read s);*)
			if debug then print ();

			let rec calc x =
				if St.is_empty adj.(abs x - 1) then ()
				else begin
					cnx.(abs x - 1) <- Some (next (), Cor.read x);
					St.iter (fun y -> match cnx.(abs y - 1) with
						|None ->
							(*adj.(x - 1) <- St.remove y adj.(x - 1);
							adj.(y - 1) <- St.remove x adj.(y - 1);*)
							if debug then print ();
							calc y;
							if debug then print ();
						|Some(a, b) -> if debug then print()
					) adj.(abs x - 1);
					St.iter (fun y -> receive x y) adj.(abs x - 1);
				end
			in

	calc s;
	if Cor.graph then Print.file Cor.var lst cnx;
	match cnx.(abs t - 1) with
		|None -> failwith "Graph: calc"
		|Some(a, b) -> b

end;;

module type GraphAbstract = functor (Cor : CoreElt) ->
sig
	type graph
	val create : unit -> graph
	val add : int -> int list -> graph -> graph
	val find : int -> int -> graph -> int
end;;

module Make = (GraphCore : GraphAbstract);;


(* Test *)


module Graph = Make (struct
		let var  = 10
		let graph = true
		let read x = x
	end);;

let g = Graph.create ();;
let g = Graph.add 1 [-2] g;;
let g = Graph.add (-2) [3; -4] g;;
let g = Graph.add 3 [-4] g;;
let g = Graph.add (-4) [5; -6; 7] g;;
let g = Graph.add 5 [-6; 7] g;;
let g = Graph.add (-6) [7] g;;
let g = Graph.add 7 [-8] g;;
let g = Graph.add (-8) [] g;;
Graph.find 1 8 g;;

(*
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

(* Old *)

(*
let rec calc x =
	if St.is_empty adj.(x - 1) then ()
	else begin
		let y = St.choose adj.(x - 1) in
			adj.(x - 1) <- St.remove y adj.(x - 1);
			adj.(y - 1) <- St.remove x adj.(y - 1);
			if debug then begin
				print_string "Adj: ";
				Array.iteri (fun i s -> if i > 0 then print_string "; ";
					print_list (St.elements s)) adj;
				print_newline();
				print_string "Cnx: ";
				Array.iteri (fun i c -> if i > 0 then print_string "; ";
					print_some c) cnx;
				print_newline();
				print_newline()
			end; 
			begin match cnx.(x - 1) with
				|None -> cnx.(x - 1) <- Some (next (), Cor.read x)
				|Some(a, b) -> () end;
			calc y;
			receive x y;
			calc x;
	end
and receive x y = begin match cnx.(x - 1), cnx.(y - 1) with
	|_, None -> ()
	|None, Some(c, d) -> failwith "Graph"
	|Some(a, b), Some(c, d) -> cnx.(x - 1) <-
		(if a < c then Some(a, b) else Some(c, d)) end;		
		if debug then begin
			print_string "Adj: ";
			Array.iteri (fun i s -> if i > 0 then print_string "; ";
				print_list (St.elements s)) adj;
			print_newline();
			print_string "Cnx: ";
			Array.iteri (fun i c -> if i > 0 then print_string "; ";
				print_some c) cnx;
			print_newline();
			print_newline()
		end; in
*)
