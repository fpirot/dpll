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
  val graphe : cls -> (int * int) list
end;;

module Print = functor (Cor : CoreElt) ->
struct

  let _ = Sys.command "if [ -d Graph/ ]; then : ; else mkdir Graph/; fi"

  let compt = ref 0

  let fresh () = incr compt; !compt

  let colors channel arr =
    Printf.fprintf channel "\nfalse[style=filled,shape=box,color=crimson];";
    Array.iteri (fun i x -> if x then
	Printf.fprintf channel "\n%d[style=filled,color=cornflowerblue];" (Cor.read (i+1))) arr 
  (* Attribue leur couleur aux noeuds du graphe. *)  
    
  let rec node channel test = function
    |[] -> ()
    |(x, y):: l -> if test x y then
  Printf.fprintf channel "\n%d -> %d;" (Cor.read x) (Cor.read y); node channel test l

  let file cls arr lst =
    let test =
      let mat = Array.create_matrix Cor.var Cor.var true in
      fun x y -> let b = abs x <> abs y && mat.(abs x - 1).(abs y - 1) in mat.(abs x - 1).(abs y - 1) <- false; b in
    let channel =
      open_out ("Graph/graph"^(string_of_int (fresh ()))^".dot") in
    Printf.fprintf channel "digraph G {\nsize =\034%d, %d\034;" Cor.var Cor.var;
    colors channel arr;
    Printf.fprintf channel "\nsubgraph current_level{";
    Cor.iter (fun x -> Printf.fprintf channel "\n%d -> false" (Cor.read x)) cls;
    node channel test lst;
    Printf.fprintf channel "\n}";
    close_out channel;

end;;

module GraphCore = functor (Cor : CoreElt) ->
struct

  module Print = Print(Cor)

  (* Du debug a la pelle *)
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


  let draw cls lst =
    (* Tableau de booleen indiquant si une variable a ete assigne au niveau courant *)
    let arr = 
      
      let ar = Array.make Cor.var false
      and current =
        let max = ref min_int in
          Cor.iter (fun x -> if (Cor.depth x) > !max then max := (Cor.depth x)) cls;
          !max in
      
      List.iter (fun (x, y) -> if (Cor.depth x) = current then ar.(abs x - 1) <- true;
        if (Cor.depth y) = current then ar.(abs y - 1) <- true) lst;
        
      ar in
   Print.file cls arr lst
      
end;;

module type GraphAbstract = functor (Cor : CoreElt) ->
sig
	type cls = Cor.cls
	val draw : cls -> (int * int) list  -> unit 
end;;

module Make = (GraphCore : GraphAbstract);;

(* Old *)

(*let draw cls =

    let arr = Array.make Cor.var false
    and partition a l lst =
      let c = Cor.father a
      and max = ref min_int in
      Cor.iter (fun x -> if (Cor.depth x) > !max then max := (Cor.depth x)) c;
      (Cor.cls_fold (fun x ((l1, l2), l3) -> if x <> a && (Cor.depth x) = !max then
	  (((x, a)::l1, l2), x::l3) else ((l1, (x, a)::l2), l3)) c (lst, l)) in

    let rec iterate lst = function
      |[] -> lst
      |a::l -> arr.(abs a - 1) <- true;
	let (lst', l') = partition a l lst in
	iterate lst' l'

    in
    let lst = iterate ([],[]) (Cor.literals cls) in
    Print.file cls arr lst
*)
(*
let rec others channel test = function
  |[] -> ()
  |(x, y):: l -> if test x y then
Printf.fprintf channel "\n%d -> %d;" (Cor.read x) (Cor.read y); others channel test l

let rec currents channel test = function
  |[] -> ()
  |(x, y):: l -> if test x y then
Printf.fprintf channel "\n%d -> %d;" (Cor.read x) (Cor.read y); currents channel test l
*)
