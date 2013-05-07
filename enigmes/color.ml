type graph = {n : int; node : int array; edge : int list array};;

(* ************************************************************************** *)
(*                      Fonction de lecture de graphe                         *)
(* ************************************************************************** *)

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


(* ************************************************************************** *)
(*                Réduction du problème en une instance de SAT                *)
(* ************************************************************************** *)

type sat = {var : int; mutable p : int; mutable clauses : int list list}
(* n = nb de variables, p = nb de clauses *)

let rec string_of_clause = function
  | [] -> "0\n"
  | x :: r -> (string_of_int x)^" "^(string_of_clause r);;

let output_sat file sat =
  let n = sat.var
  and p = List.length sat.clauses in
output_string file ("p cnf "^(string_of_int n)^" "^(string_of_int p)^"\n");
  let c = ref sat.clauses in
for i = 1 to p do
  let s = string_of_clause (List.hd !c) in
  output_string file s; c := List.tl !c
done;;

let reduction k g =
  let exists_color i = 
    let rec aux i c = if c = 0 then []
      else k*i + c - 1 :: (aux i (c - 1)) in
    aux i k in
  let sat = {var = k*g.n; p = 0; clauses = []} in
  (* On introduit pour chaque noeud i du graphe k variables x_ic =
     k*i+c, signifiant "le noeud i a la couleur c". *)
  for i = 0 to g.n - 1 do
    for c = 0 to k - 1 do
      List.iter (fun j -> sat.p <- sat.p + 1; sat.clauses <- [-(k*i + c); -(k*j + c)] :: sat.clauses) g.edge.(i);
    done;
    sat.p <- sat.p + 1; sat.clauses <- (exists_color i) :: sat.clauses
  (* On rajoute la clause signifiant que le noeud i doit avoir une
     couleur. *)
  done;
  sat;;

let color g t k =
  for i = 0 to g.n - 1 do
    for c = 0 to k - 1 do
      if t.(k*i+c) > 0 then g.node.(i) <- c
    done
  done;;

let main () =
  let channel = Scanf.Scanning.open_in (try Sys.argv.(1) with _ -> "../Test/graph.cnf") in
  let (n, edge) = load channel in
  let g = {n = n; node = Array.make n (-1); edge = edge} in
  let k = try int_of_string Sys.argv.(2) with _ -> 3 in
  let sat = reduction k g in
  let file = open_out "../Test/color.cnf" in
output_sat file sat;
flush file;
Sys.command "./../dpll ../Test/color.cnf";;
