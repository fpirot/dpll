let latin_hor n j = 
  let m = Array.make n [||] in
  for i = 0 to n-1 do
    m.(i) <- Array.init n (fun k -> (i * j + k) mod n)
  done;
m;;

let latin_vert n i = 
  let m = Array.make_matrix n n 0 in
  for k = 0 to n-1 do
    for j = 0 to n-1 do
      m.(k).(j) <- (i * j + k) mod n
    done
  done;
  m;;

let magique n =
  let (m,m') = (latin_hor n 3, latin_vert n 3) in
  for i = 0 to n-1 do 
    for j = 0 to n-1 do
      m.(i).(j) <- n * m.(i).(j) + m'.(i).(j) + 1
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

let print_matrix m =
    Array.iter (fun vect -> Array.iter (fun x -> printint x; print_string " ") vect; print_newline()) m;;

let main () =
  let n = try 2 * (int_of_string Sys.argv.(1)) + 1 with _ -> 3 in 
  print_matrix (magique n);;

main();;
