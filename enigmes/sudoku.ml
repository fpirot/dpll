module Solution = Solution.Make;;

let read file i =
  for k = 2 to i do Scanf.bscanf file "%s\n" (fun x -> ()) done;
  let s = Scanf.bscanf file "%s\n" (fun x -> x) in
  let rec aux i l s = 
    try Scanf.sscanf s "%c%s" (fun c s -> let k = int_of_string (String.make 1 c) in
					  if k > 0 then aux (i+1) (i + 81*(k-1) :: l) s
					  else aux (i+1) l s)
    with End_of_file -> l in
  aux 1 [] s
(* Lit une grille partiellement remplie de sudoku, et renvoie la liste des variables contraintes dans la réduction. *)


let reduction m lst =
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
	for x = i+1 to n-1 do
	  (* Nombres différents sur une même colonne. *)
	  Solution.add_clause [-(i + n*j + n*n*k + 1); -(x + n*j + n*n*k + 1)] done;
	for x = j+1 to n-1 do
	  (* Nombres différents sur une même ligne. *)
	  Solution.add_clause [-(i + n*j + n*n*k + 1); -(i + n*x + n*n*k + 1)] done;
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
		(* Pour tout sous-carré de la grille, on veut des nombres distincts. *)
		if x <> z && y <> t then Solution.add_clause [- (m*i + x + n*(m*j + y) + n*n*k + 1); - (m*i + z + n*(m*j + t) + n*n*k + 1)]
	      done
	    done
	  done
	done
      done
    done
  done;
  List.iter (fun x -> Solution.add_clause [x]) lst;;
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

let printint x = print_int x; print_char ' ';;

let transpose t =
  let n,p = Array.length t, Array.length t.(0) in
  let t' = Array.make_matrix p n t.(0).(0) in
  for i = 0 to p-1 do
    for j = 0 to n-1 do
      t'.(i).(j) <- t.(j).(i)
    done
  done;
  t';;
(* Nos variables sont croissantes le long des colonnes, puis le long
   des lignes, et on veut considérée l'entrée comme étant lue de manière
   croissante le long des lignes, puis des colonnes. On travaille donc
   tout du long avec la transposé du problème, que l'on retourne à la
   fin. *)

let print_solution (b,t) n =
  if b then begin
    print_string "SATISFIABLE\n";
    let rec line n = if n = 0 then print_newline() else (print_string "-"; line (n-1)) in 
    let k = ref 0 in
    line 34;
    Array.iter (fun vect -> Array.iter (fun x -> if !k mod 3 = 0 then print_string "| "; incr k; 
      printint x; print_string " ") vect; 
      print_string "|\n"; if !k mod 27 = 0 then line 34) (transpose (solution t n))
  end
  else print_string "UNSATISFIABLE\n";;

let main () =
  let n = try int_of_string Sys.argv.(1) with _ -> 1 in
  let l = read (Scanf.Scanning.open_in "grilles.txt") n in
  reduction 3 l;
  let file = open_out "../Test/latin.cnf" in
  Solution.write file;
  let _ = Sys.command "./../dpll -naff -moms ../Test/latin.cnf" in
  let (b,t) = Solution.read (Scanf.Scanning.open_in "../Test/result.txt") in
  print_solution (b,t) 3;;

main();;
