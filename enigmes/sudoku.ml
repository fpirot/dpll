module Solution = Solution.Make;;

let reduction m =
  let n = m*m in
  let exists_number i j = 
    let rec aux p k = if k = 0 then [p]
      else (p + n*n*k) :: (aux p (k - 1)) in
    aux (i + n*j + 1) (n-1) in
  Solution.fix (n*n*n);
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
	(* variable x_ijk = i + n*j + n^2*k + 1 pour "la case i j contient la valeur k+1" *)
	for x = i+1 to n-1 do Solution.add_clause [-(i + n*j + n*n*k + 1); -(x + n*j + n*n*k + 1)] done;
	(* Nombres différents sur une même colonne. *)
	for x = j+1 to n-1 do Solution.add_clause [-(i + n*j + n*n*k + 1); -(i + n*x + n*n*k + 1)] done;
      (* Nombres différents sur une même ligne. *)
      done;
      Solution.add_clause (exists_number i j)
    done;
  done;
  for i = 0 to m-1 do
    for j = 0 to m-1 do
      for k = 0 to n-1 do
	for x = 0 to m-1 do
	  for y = 0 to m-1 do
	    for z = 0 to m-1 do
	      for t = 0 to m-1 do
		if x <> z && y <> t then Solution.add_clause [- (m*i + x + n*(m*j + y) + n*n*k + 1); - (m*i + z + n*(m*j + t) + n*n*k + 1)]
	      (* Pour tout sous-carré de la grille, on veut des nombres distincts. *)
	      done
	    done
	  done
	done
      done
    done
  done;;
(* Remplit une instance de sat qui traduit les contraintes d'un sudoku de côté m *)

let solution t n =
  let m = Array.make_matrix (n*n) (n*n) 0 in
  for i = 0 to (n*n) - 1 do
    for j = 0 to (n*n) - 1 do
      let k = ref 0 in
      while (!k < n*n && t.(i + n*n*j + n*n*n*n* !k) < 0) do incr k done;
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
  let n = try int_of_string Sys.argv.(1) with _ -> 3 in
  reduction n;
  let file = open_out "../Test/latin.cnf" in
  Solution.write file;
  let _ = Sys.command "./../dpll -naff -dlis ../Test/latin.cnf" in
  let (b,t) = Solution.read (Scanf.Scanning.open_in "../Test/result.txt") in
  print_solution (b,t) n;;

main();;
