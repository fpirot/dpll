module Solution = Solution.Make;;

let n = try int_of_string Sys.argv.(1) with _ -> 5;;
let x i j k = 2 * (i + n*j + n*n*k + 1);;
let y i j k = 2 * (i + n*j + n*n*k + 1) - 1;;


let reduction () =
  let exists_numberx i j = 
    let rec aux k = if k = 0 then [x i j 0]
      else (x i j (k-1)) :: (aux (k - 1)) in
    aux n
  and exists_numbery i j = 
    let rec aux k = if k = 0 then [y i j 0]
      else (y i j (k-1)) :: (aux (k - 1)) in
    aux n in
  Solution.fix (2*n*n*n);
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
	(* variable x_ijk = i + n*j + n^2*k + 1 pour "la case i j contient la valeur k+1" *)
	for l = i+1 to n-1 do 
	  Solution.add_clause [- x i j k; - x l j k];
	  Solution.add_clause [- y i j k; - y l j k] done;
	(* Nombres différents sur une même colonne. *)
	for l = j+1 to n-1 do 
	  Solution.add_clause [- x i j k; - x i l k]; 
	  Solution.add_clause [- y i j k; - y i l k];done;
	(* Nombres différents sur une même ligne. *)
	for k' = 0 to n-1 do
	  for l = i+1 to n-1 do
	    for m = 1 to n-1 do
	      if j <> m then Solution.add_clause [- x i j k; - y i j k'; - x l m k; - y l m k']
	    done
	  done
	done
      done;
      Solution.add_clause (exists_numberx i j); 
      Solution.add_clause (exists_numbery i j)
    done;
    for j = i+1 to n-1 do
      for k = 0 to n-1 do
	Solution.add_clause [- x i i k; - x j j k];
	Solution.add_clause [- y i i k; - y j j k];
	Solution.add_clause [- x i (n-1-i) k; - x j (n-1-j) k];
	Solution.add_clause [- y i (n-1-i) k; - y j (n-1-j) k]
      done
    done 
  done;;
(* Remplit une instance de sat qui traduit les contraintes d'un carré latin de côté n *)

let solution t =
  let (m, m') = (Array.make_matrix n n 0, Array.make_matrix n n 0) in
  if Array.length t <> n then failwith "Too hard instance!";
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      let k = ref 0 in
      while (!k < n && t.(x i j !k - 1) < 0) do incr k done;
      m.(i).(j) <- !k + 1;
      k := 0;
      while (!k < n && t.(y i j !k - 1) < 0) do incr k done;
      m'.(i).(j) <- !k + 1;
    done
  done;
  (m, m');;

let transformation (m, m') =
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      m.(i).(j) <- n * (m.(i).(j) - 1) + m'.(i).(j)
    done
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

let print_solution (b,t) =
  if b then begin
    print_string "SATISFIABLE\n";
    Array.iter (fun vect -> Array.iter (fun x -> printint x; print_string " ") vect; print_newline()) (transformation (solution t))
  end
  else print_string "UNSATISFIABLE\n";;

let main () =
  reduction ();
  let file = open_out "../Test/latin.cnf" in
  Solution.write file;
  let _ = Sys.command "./../dpll -naff -dlis ../Test/latin.cnf" in
  let (b,t) = Solution.read (Scanf.Scanning.open_in "../Test/result.txt") in
  print_solution (b,t);;

main();;
