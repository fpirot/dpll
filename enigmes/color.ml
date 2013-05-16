type graph = {n : int; node : int array; edge : int list array};;

module Solution = Solution.Make;;

module Print =
struct

  let t = [|"red"; "blue"; "green"; "gold"; "orange"; "rose"; "purple"; "brown"; "white"; "green4"; "grey"|]

  let node channel graph =
    Array.iteri (fun i x -> Printf.fprintf channel "\n%d[style=filled,color=%s];" i t.(x mod 11)) graph.node 
      
  let edge channel graph =
    Array.iteri (fun i -> List.iter (fun j -> if i < j then Printf.fprintf channel "\n%d -- %d" i j)) graph.edge

    let draw graph =
      let channel = open_out "color.dot" in
        Printf.fprintf channel "graph G {\nsize =\034%d, %d\034;" graph.n graph.n;
        node channel graph;
        edge channel graph;
        Printf.fprintf channel "\n}";
        close_out channel

end;;


(* ************************************************************************** *)
(*                      Fonction de lecture de graphe                         *)
(* ************************************************************************** *)

let edge channel graph = Scanf.bscanf channel " %d %d\n"
  (fun u v -> graph.(u-1) <- (v-1) :: (graph.(u-1)); graph.(v-1) <- (u-1) :: (graph.(v-1)))

let rec read channel graph = function
  |0 -> graph
  |n -> Scanf.bscanf channel "%c"
    (fun c -> match c with
      |'e' -> edge channel graph; read channel graph (n-1)
      |_ -> Scanf.bscanf channel "%[^\n]\n" (fun _ -> ()); read channel graph n)

let init channel n e =
  let graph = Array.make n [] in
  read channel graph e

let load channel =
  Scanf.bscanf channel "p edge %d %d\n" (fun n e -> n, init channel n e)


(* ************************************************************************** *)
(*                Réduction du problème en une instance de SAT                *)
(* ************************************************************************** *)


let reduction k g =
  let exists_color i = 
    let rec aux i c = if c = 0 then [k*i + 1]
      else k*i + c + 1 :: (aux i (c - 1)) in
    aux i (k-1) in
  Solution.fix (k*g.n);
  (* On introduit pour chaque noeud i du graphe k variables x_ic =
     k*i+c + 1, signifiant "le noeud i a la couleur c". *)
  for i = 0 to g.n - 1 do
    for c = 0 to k - 1 do
      List.iter (fun j -> Solution.add_clause [-(k*i + c + 1); -(k*j + c + 1)]) g.edge.(i);
    done;
    Solution.add_clause (exists_color i)
  (* On rajoute la clause signifiant que le noeud i doit avoir une couleur. *)
  done;;

let color g t k =
  for i = 0 to g.n - 1 do
    for c = 0 to k - 1 do
      if t.(k*i+c) > 0 then g.node.(i) <- c
    done
  done;;

let print_solution (b,t) g k = if b then begin
  print_string "SATISFIABLE\n";
  color g t k;
  Print.draw g
end
  else print_string "UNSATISFIABLE";;

let main () =
  let channel = Scanf.Scanning.open_in (try Sys.argv.(1) with _ -> "../Test/graph0.cnf") in
  let (n, edge) = load channel in
  let g = {n = n; node = Array.make n (-1); edge = edge} in
  let k = try int_of_string Sys.argv.(2) with _ -> 3 in
  reduction k g;
  let file = open_out "../Test/color.cnf" in
  Solution.write file;
  let _ = Sys.command "./../dpll -naff ../Test/color.cnf" in
  print_solution (Solution.read (Scanf.Scanning.open_in "../Test/result.txt")) g k;
  let _ = Sys.command "dot -Tps color.dot -o color.ps" in ();;

main ();;
