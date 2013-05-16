module Solution =
struct

  type sat = {mutable var : int; mutable p : int; mutable clauses : int list list}
  (* var = nb de variables, p = nb de clauses *)

  let sat = {var = 0; p = 0; clauses = []}

  let fix n = sat.var <- n

  let add_clause l = sat.p <- sat.p + 1; sat.clauses <- l :: sat.clauses

  let rec string_of_clause = function
    | [] -> "0\n"
    | x :: r -> (string_of_int x)^" "^(string_of_clause r)

  let write file = 
    let n = sat.var
    and p = List.length sat.clauses in
    output_string file ("p cnf "^(string_of_int n)^" "^(string_of_int p)^"\n");
    let c = ref sat.clauses in
    for i = 1 to p do
      let s = string_of_clause (List.hd !c) in
      output_string file s; c := List.tl !c
    done;
    flush file

  let read file =
    let b = ref true in
    let s = try Scanf.bscanf file "%s\n" (fun x -> x) with _ -> failwith "Parsing error while reading result.txt" in
    (match s with 
      | "SATISFIABLE" -> b := true
      | "UNSATISFIABLE" -> b := false
      | _ -> failwith "Parsing error while reading result.txt");
    let n = try Scanf.bscanf file "var %d\n" (fun x -> x) with _ -> 0 in
    let t = Array.make n 0 in
    for i = 0 to n-1 do t.(i) <- Scanf.bscanf file "%d " (fun x -> x) done;
    (!b,t)

end;;

module type Solution_abstract =
sig
  val fix : int -> unit
  val add_clause : int list -> unit
  val write : out_channel -> unit
  val read : Scanf.Scanning.in_channel -> bool * int array
end;;

module Make = (Solution : Solution_abstract);;
