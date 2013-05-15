type sat = {var : int; mutable p : int; mutable clauses : int list list}
(* n = nb de variables, p = nb de clauses *)

let reduction n =
  let exists_number i j = 
    let rec aux p k = if k = 0 then []
      else p*n + k - 1 :: (aux p (k - 1)) in
    aux (i+n*j) n in
  let sat = {var = n*n*n; p = 0; clauses = []} in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
	(* variable x_ijk = i + n*j + n^2*k pour "la case i j contient la valeur k+1" *)
	for x = i+1 to n-1 do sat.p <- sat.p + 1; sat.clauses <- [-(i + n*j + n*n*k); -(x + n*j + n*n*k)] :: sat.clauses done;
	(* Nombres différents sur une même colonne. *)
	for x = j+1 to n-1 do sat.p <- sat.p + 1; sat.clauses <- [-(i + n*j + n*n*k); -(i + n*x + n*n*k)] :: sat.clauses done;
      (* Nombres différents sur une même ligne. *)
      done;
      sat.p <- sat.p + 1; sat.clauses <- exists_number i j :: sat.clauses
    done;
  done;
sat;;
(* Renvoie une instance de sat qui traduit les contraintes d'un carré latin de côté n *)

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

let read_solution file = 
  let b = ref true in
  let s = Scanf.bscanf file "%s\n" (fun x -> x) in
  (match s with 
    | "SATISFIABLE" -> b := true
    | "UNSATISFIABLE" -> b := false
    | _ -> failwith "Parsing error while reading result.txt");
  let n = if !b then Scanf.bscanf file "var %d\n" (fun x -> x) else 0 in
  let t = Array.make n 0 in
  for i = 0 to n-1 do t.(i) <- Scanf.bscanf file "%d " (fun x -> x) done;
  (!b,t);;

let solution t n =
  let m = Array.make_matrix n n 0 in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      let k = ref 0 in
      while (!k < n && t.(i + n*j + n*n* !k) < 0) do incr k done;
      m.(i).(j) <- !k + 1
    done;
  done;
  m;;

let printint x =
  let e = if x < 0 then 1 else 0 in
  let s = int_of_float (log (float (abs x)) /. log 10.) in
  let n = 2 - s - e in
  let rec space = function
    |n when n < 0 -> ()
    |0 -> print_char ' '
    |n -> print_char ' '; space (n-1) in
  print_int x; space n
(* Affiche un entier sur un nombre de caractères fixé, en
   complétant avec des espaces. *)

let print_solution (b,t) n =
  if b then begin
    print_string "SATISFIABLE\n";
    Array.iter (fun vect -> Array.iter (fun x -> printint x; print_string " ") vect; print_newline()) (solution t n)
  end
  else print_string "UNSATISFIABLE\n";;

let main () =
  let n = try int_of_string Sys.argv.(1) with _ -> 10 in
  let sat = reduction n in
  let file = open_out "../Test/latin.cnf" in
  output_sat file sat;
  flush file;
  let _ = Sys.command "./../dpll ../Test/latin.cnf" in
  flush_all();
  let (b,t) = read_solution (Scanf.Scanning.open_in "../Test/result.txt") in
  print_solution (b,t) n;;

main();;
