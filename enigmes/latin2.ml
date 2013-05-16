module Solution = Solution.Make;;

let reduction n =
  let appears_number_line i k = 
    let rec aux j = if j = 0 then [i + n*n*k +1]
      else (i + n*j + n*n*k + 1) :: (aux (j-1))
    in aux (n-1) in
  let appears_number_colonne j k = 
    let rec aux i = if i = 0 then [n*j + n*n*k +1]
      else (i + n*j + n*n*k + 1) :: (aux (i-1))
    in aux (n-1) in
  Solution.fix (n*n*n);
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
	for l = k+1 to n-1 do
	  Solution.add_clause [- (i + n*j + n*n*k + 1); - (i + n*j + n*n*l + 1)]
	done
      done
    done
  done;
  for k = 0 to n-1 do
    for x = 0 to n-1 do
      Solution.add_clause (appears_number_line x k);
      Solution.add_clause (appears_number_colonne x k)
    done
  done;;
(* Remplit une instance de sat qui traduit les contraintes d'un carré latin de côté n *)

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
  reduction n;
  let file = open_out "../Test/latin.cnf" in
  Solution.write file;
  let _ = Sys.command "./../dpll -naff -moms ../Test/latin.cnf" in
  let (b,t) = Solution.read (Scanf.Scanning.open_in "../Test/result.txt") in
  print_solution (b,t) n;;

main();;
