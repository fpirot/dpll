(* Module de, surprise, chargement des donnees *)

module Load =
struct

  exception Useless

  let rec insert x = function
    | [] -> [(0, x)]
    | (m, y) :: l ->
      if x = y then
	(m + 1, y) :: l
      else
	match insert x l with
	  | [] -> failwith "Insert"
	  | (m', y') :: l' ->
	    if m' > m then
	      (m', y') :: (m, y) :: l'
	    else
	      (m, y) :: (m', y') :: l'


  let add x l = if List.exists (fun y -> y = -x) l then raise Useless else x::l


  let rec read s channel = Scanf.bscanf channel "%0c"
    (fun c -> if c = 'c' then
	read (Scanf.bscanf channel "%[^\n]\n" (fun x -> s^ x ^ "\n")) channel
      else s)


  let init str n channel =
    let rec next () = match Scanf.bscanf channel " %d " (fun x -> x) with
      | 0 -> ()
      | n -> next ()
	
    and iter s c lst lstC ensV = function
      | 0 -> let s = read s channel in Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then (c + 1, ensV :: lstC, lst, s)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) 0 with
		       Useless -> (c, lstC, lst, s) end)
      | n -> let s = read s channel in Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then
		   iter s c lst (ensV :: lstC) [] (n - 1)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) n with
		       Useless ->
			 (next (); iter s (c - 1) lst lstC [] (n - 1)) end) in
    
    iter str n [] [] [] n;;


  let load channel =
    let s = read "" channel (*try Scanf.bscanf channel "c %s@\n" (fun x -> x ^ "\n") with _ -> ""*) in
    Scanf.bscanf channel "p cnf %d %d" (fun v c -> (v, init s (c-1) channel))

end;;


(* Regroupe les modules d'initialisation *)

module Core =
struct

  type cls = int
  (* On repère une clause par son indice dans un tableau dynamique. *)
  exception Satisfiable
  exception Unsatisfiable of cls

  let debug = true

  let printint x =
    let e = if x < 0 then 1 else 0 in
    let s = int_of_float (log (float (abs x)) /. log 10.) in
    let n = 3 - s - e in
    let rec space = function
      |n when n < 0 -> ()
      |0 -> print_char ' '
      |n -> print_char ' '; space (n-1) in
    print_int x; space n
  (* Affiche un entier sur un nombre de caractères fixé à 4, en
     complétant avec des espaces. *)

  let print_list l =
    let rec print = function
      |[] -> print_string "]"
      |[a] -> print_int a; print_string "]"
      |a::l -> print_int a; print_string "; "; print l in
    print_string "["; print l

  let (wlit, graph, heur, path) =
    let w = ref false
    and g = ref false
    and s = ref "Nil"
    and p = ref "Test/ex0.cnf" in
    Arg.parse [("-wlit", Arg.Unit(fun () -> w := true), "Watched literals");
               ("-graph", Arg.Unit(fun () -> g := true), "Watched literals"); 
	       ("-rand", Arg.Unit(fun () -> s := "Rand"), "Random selection");
               ("-moms", Arg.Unit(fun () -> s := "Moms"), "Maximum Occurrences in clauses of Minimum Size");
               ("-dlis", Arg.Unit(fun () -> s := "Dlis"), "Dynamic Largest Individual Sum")]
      (fun str -> p := str) "";
    (!w, !g, !s, !p)
      
  let (var, (cls, lst, ord, comment)) = Load.load (Scanf.Scanning.open_in (path))
    
  let fold = List.fold_right

  (* ********************************************************* *)
  (*        Gestion des affectations et des backtracks         *)
  (* ********************************************************* *)
      
  let stack = Array.make var []
  (* La "pile" contenant les assignations successives : on utilise un
     tableau de liste en partant du principe qu'il y a au plus n étages
     de paris, avec n le nombre de variables. *)

  let dpth = ref 0
  (* La profondeur de paris en cours. *)

  let fix_depth i = dpth := i;
    if debug then begin
      print_string "Stack: ";
      for k = 0 to i-1 do print_list stack.(k) done;
      print_newline() end

  type assig = {mutable value: int; mutable father: cls; mutable depth: int}
  (* Le type assig contient comme information la valeur de vérité
     value prise par le litéral (0 si indéfinie), la clause qui a
     engendré son assignation (-1 si c'est un pari), et la profondeur
     depth de son assignation dans la pile de propagation. *)

  let zero = {value = 0; father = -1; depth = 0}

  let assigArray = let t = Array.make var zero in
		   for i = 0 to var-1 do t.(i) <- {value = 0; father = -1; depth = 0} done; t
		     
  let reset x = assigArray.((abs x) - 1) <-  {value = 0; father = -1; depth = 0};
    if debug then begin
      print_string "Assignment:\n";
      Array.iter (fun x -> printint x.value; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.father; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.depth; print_char ' ') assigArray;
      print_string "\n\n" end
  (* Réinitialise la valuation de la variable |x|. *)

  let propag x = stack.(!dpth) <- (x :: stack.(!dpth))

  let restore i = 
    if debug then begin
      print_string "Restore: ";
      for k = 0 to !dpth do (fun l -> print_list l) stack.(k) done;
      print_newline() end;
    for k = i to !dpth do
      List.iter reset stack.(k);
      stack.(k) <- [] done
  (* Annule les assignations effectuées aux profondeurs plus grandes que
     i *)

  let read x = assigArray.((abs x) - 1).value

  let write ?(father = -1) x = assigArray.((abs x) - 1).value <- x;
    assigArray.((abs x) - 1).depth <- !dpth;
    assigArray.((abs x) - 1).father <- father;
    propag x;
    if debug then begin
      print_string "Assignment:\n";
      Array.iter (fun x -> printint x.value; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.father; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.depth; print_char ' ') assigArray;
      print_string "\n\n" end
  (* Ecrit la valuation x pour la variable |x|, avec en argument
     optionnel la clause à l'origine de cette valuation. *)

  let father x = assigArray.((abs x) - 1).father
  (* Renvoie la clause qui a engendré la valuation x. *)

  let depth x = assigArray.((abs x) - 1).depth
  (* Renvoie la profondeur à laquelle x a été assigné. *)

  let write_father x c = assigArray.((abs x) - 1).father <- c;
    if debug then begin
      print_string "Assignment:\n";
      Array.iter (fun x -> printint x.value; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.father; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.depth; print_char ' ') assigArray;
      print_string "\n\n" end

  (* ********************************************************* *)
  (*          Référencement des clauses du problème            *)
  (* ********************************************************* *)

  module Cls = Set.Make
    (struct
      type t = int
      let compare x y = compare (abs x) (abs y)
     end)
  (* Les clauses sont des ensembles d'entiers (+x pour le litéral vrai
     de la variable x, -x pour sa négation), avec la relation de
     comparaison sur les valeurs absolues (entre nom de variable). *)

  type clause = Cls.t

  module ClauseArray = Da.Make (Cls)

  let clauseArray = ClauseArray.empty
  (* On référencie l'ensemble des clauses dans un tableau, afin de
     stocker des indices dans nos structures de données plutôt que des
     clauses. *)

  let compt = ref (-1)
  (* L'indice en cours dans le tableau. *)
(*
  let add_clause c = incr compt; clauseArray.(!compt) <- c; !compt
*)

  let add_clause c = ClauseArray.add c clauseArray; ClauseArray.length clauseArray - 1
(*
  let fill l =
    incr compt;
    clauseArray.(!compt) <- fold (fun x s -> Cls.add x s) l Cls.empty;
    !compt
*)  
  let fill l = ClauseArray.add (fold (fun x c -> Cls.add x c) l Cls.empty) clauseArray;
    ClauseArray.length clauseArray - 1
(* Renvoie dans la case du tableau en cours la clause représentée par sa liste d'entiers l. *)
(*
  let clause id = clauseArray.(id)
*)
  let clause id = if id = -1 then Cls.empty else ClauseArray.read id clauseArray

  let cls_make id = id

  let cls_fold f cls = Cls.fold f (clause cls)
  
  let iter f cls = Cls.iter f (clause cls)

  let length cls = cls_fold (fun x t -> if read x = 0 then t+1 else t) cls 0

  let is_singleton cls = 
    try (
      match Cls.fold 
	(fun x v -> match (read x, v) with
          |(0, 0) -> x
          |(0, _) -> failwith "is_not"
	  |(_, y) ->  y) (clause cls) 0
      with
	|0 -> if debug then begin
	  print_string "Clause insatisfiable: ";
	  print_list (Cls.elements (clause cls));
	  print_newline() end;
	  raise (Unsatisfiable cls)
	|x -> x
    )
    with Failure "is_not" -> 0
  (* Renvoie l'unique élément de la clause d'indice id qui n'est pas
     encore assigné quand il est bien unique, 0 sinon. Lève
     l'exception Unsatisfiable si la clause n'est pas satisfiable.*)

  let literals cls = Cls.elements (clause cls)
  (* Donne les elements d'une clause *)
      
  let choose cls = Cls.choose (clause cls)


  (* ************************************************* *)
  (*        Gestion intelligente du backtrack          *)
  (* ************************************************* *)

  module Proof = struct
    type t = F of Cls.t | N of Cls.t * Cls.t * t
    let singleton x = F(x)
    let hd = function
      |F(x) -> x
      |N(c, _, _) -> c
    let built c1 c2 p = N(c1, c2, p)
  end

  type proof = Proof.t

  let proof c = 
    let add = Cls.fold (fun x s -> if (depth x = !dpth && father x <> -1) then (write_father x (-1); Cls.add x s) else s) in
    (* Rajoute à un ensemble tous les litéraux d'une clause qui ont
       été affectés pendant la propagation en cours. *)
    let rec aux p s = 
      (* s est un ensemble de litéraux; on utilisera les opérations
	 sur les clauses pour le manipuler. *)
      if Cls.is_empty s then p
      else let x = Cls.choose s in
	   let s1 = Cls.remove x s in
	   if Cls.is_empty s1 then p
	   (* On s'arrête quand on a trouvé un point d'articulation. *)
	   else let c1 = clause (father x) in
		let s2 = add c1 s1 in
		aux (Proof.built (Cls.union c1 (Cls.remove x (Proof.hd p))) c1 p) s2 in
    (* Renvoie la clause engendrée par le backtrack. *)
    aux (Proof.singleton c) (add c Cls.empty)
  (* Génère une preuve de résolution suite *)

  let backtrack c =
    let p = proof (clause c) in
    let c1 = Proof.hd p in
    let cls = add_clause c1
    (* On ajoute la clause ainsi créée. *)
    and d = Cls.fold (fun x d -> if depth x < !dpth then max (depth x) d else d) c1 0 in
    (* On cherche la profondeur de backtrack maximale dans cette
       clause. *)
    if debug then begin
      print_string "Clause engendrée pendant le backtrack: ";
      print_list (literals cls);
      print_string "\nNouvelle profondeur de backtrack: ";
      print_int d;
      print_newline()
    end;
    (cls, d)
(* Donne le représentant de la nouvelle clause, ainsi que la
   profondeur à laquelle le backtrack doit remonter. *)
end;;

module type Abstract =
sig
  exception Satisfiable
  type cls = int
  exception Unsatisfiable of cls
  type proof
  type clause
  val var : int
  val cls : int
  val lst : int list list
  val ord : (int * int) list
  val wlit : bool
  val graph : bool
  val heur : string
  val fix_depth : int -> unit
  val restore : int -> unit
  val read : int -> int
  val write : ?father:int -> int -> unit
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val fill : int list -> cls
  val iter : (int -> unit) -> cls -> unit
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val literals : cls -> int list
  val length : cls -> int
  val choose : cls -> int
  val is_singleton : cls -> int
  val proof : clause -> proof
  val backtrack : cls -> (cls * int)
  val father : int -> cls
end;;

module Make = (Core : Abstract);;
