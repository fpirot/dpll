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
      | 0 -> let s = read s channel in 
	     Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then (c + 1, ensV :: lstC, lst, s)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) 0 with
		       Useless -> (c, lstC, lst, s) end)
      | n -> let s = read s channel in 
	     Scanf.bscanf channel " %d "
	       (fun x -> if x = 0 then
		   iter s c lst (ensV :: lstC) [] (n - 1)
		 else begin
		   try iter s c (insert (abs x) lst) lstC (add x ensV) n with
		       Useless ->
			 (next (); iter s (c - 1) lst lstC [] (n - 1)) end)
    in iter str n [] [] [] n;;

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
    let n = 1 - s - e in
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

  let (wlit, graph, aff, smt, heur, path) =
    let w = ref false
    and g = ref false
    and aff = ref true
    and smt = ref false
    and s = ref "Nil"
    and p = ref "Test/ex0.cnf" in
    Arg.parse [("-wlit", Arg.Unit(fun () -> w := true), "Watched literals");
               ("-graph", Arg.Unit(fun () -> g := true), "Génère le graphe de propagation"); 
							 ("-rand", Arg.Unit(fun () -> s := "Rand"), "Random selection");
               ("-moms", Arg.Unit(fun () -> s := "Moms"), "Maximum Occurrences in clauses of Minimum Size");
               ("-dlis", Arg.Unit(fun () -> s := "Dlis"), "Dynamic Largest Individual Sum");
	       ("-naff", Arg.Unit(fun () -> aff := false), "Désative l'affichage en console de la solution");
	       ("-smt", Arg.Unit(fun () -> smt := true), "Travaille modulo une théorie")]
      (fun str -> p := str) "";
    (!w, !g, !aff, !smt, !s, !p)
      
  let (var, (cls, lst, ord, comment)) = Load.load (Scanf.Scanning.open_in (path))

  let ncls = ref cls

  let nb_cls () = !ncls

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

  let pari () = 
    let rec last = function
      |[] -> raise Not_found
      |[x] -> x
      |_::r -> last r
    in last stack.(!dpth)
(* Renvoie la valeur du pari à la profondeur dpth. *)

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

  let write ?(father = -1) x = 
    if read x = x then () else begin
    assigArray.((abs x) - 1).value <- x;
    assigArray.((abs x) - 1).depth <- !dpth;
    assigArray.((abs x) - 1).father <- father;
    propag x end;
    if debug then begin
      print_string "Stack: ";
      for k = 0 to !dpth do (fun l -> print_list l) stack.(k) done;
      print_newline();
      print_string "Assignment:\n";
      Array.iter (fun x -> printint x.value; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.father; print_char ' ') assigArray;
      print_newline();
      Array.iter (fun x -> printint x.depth; print_char ' ') assigArray;
      print_string "\n\n" end
  (* Ecrit la valuation x pour la variable |x|, avec en argument
     optionnel la clause à l'origine de cette valuation. *)

  let write_father x c = assigArray.((abs x) - 1).father <- c

  let father x = assigArray.((abs x) - 1).father
  (* Renvoie la clause qui a engendré la valuation x. *)

  let depth x = assigArray.((abs x) - 1).depth
  (* Renvoie la profondeur à laquelle x a été assigné. *)

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

  module ClauseArray = Da.Make (Cls)

  let clauseArray = ClauseArray.empty
  (* On référencie l'ensemble des clauses dans un tableau, afin de
     stocker des indices dans nos structures de données plutôt que des
     clauses. *)

  let compt = ref (-1)
  (* L'indice en cours dans le tableau. *)

  let add_clause c = ClauseArray.add c clauseArray; incr ncls

  let add_cls l = add_clause (List.fold_right (fun x c -> Cls.add x c) l Cls.empty); !ncls - 1

  let fill l = ClauseArray.add (fold (fun x c -> Cls.add x c) l Cls.empty) clauseArray;
    ClauseArray.length clauseArray - 1

  let clause id = if id = -1 then Cls.empty else ClauseArray.read id clauseArray

  let cls_make id = id

  let cls_fold f cls = Cls.fold (fun x v -> if read x = 0 then f x v else v) (clause cls)
  
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
    type t = Nil | N of Cls.t * t * t
    let singleton x = N(x, Nil, Nil)
    let hd = function
      |Nil -> raise Not_found
      |N(c, _, _) -> c
    let build c1 c2 p = N(c1, p, singleton c2)
    let get = function
      |Nil -> failwith "Non"
      |N(x, t1, t2) -> let l = Cls.elements x in 
		       (l, t1, t2)
    let rec size = function
      |Nil -> 0
      |N(x, t1, t2) -> 1 + size t1 + size t2
    let fils = function
      |Nil -> failwith "Non"
      |N(x, t1, t2) -> t1
  end

  type proof = Proof.t
  let size = Proof.size
  let get = Proof.get

  let calcul c =
    let t = Array.make var true in
    (* t.(abs x - 1) vaudra true jusqu'à ce que x ait été considéré. *)
    let add cls c s = Cls.fold (fun x (c,s) -> if t.((abs x) - 1) then
	begin
	  t.((abs x) - 1) <- false;
	  if depth x = !dpth then (c, Cls.add x s) else (Cls.add x c, s)
	end
      else (c,s)) cls (c,s) in
    (* Actualise la clause engendrée et l'ensemble des litéraux propagés en cours, à partir d'une clause cls. *)
    let rec aux c s p x0 g = 
      (* s est un ensemble de litéraux; on utilisera les opérations
	 sur les clauses pour le manipuler. c est la clause engendrée
	 en cours p est la preuve en cours. x0 est le pari s'il a été
	 rencontré dans la clause engendrée en cours, 0 sinon. g est
	 la liste des arêtes dans le graphe de conflits. *)
      let x = try Cls.choose s with Not_found -> 0 in
      if x = 0 then 
	if x0 <> 0 then (c, x0, p, g)
	else failwith "Problème lors de l'apprentissage de clause"
      else let s1 = Cls.remove x s in
	   (* On s'arrête quand on a trouvé un point d'articulation. *)
	   if Cls.is_empty s1 && x0 = 0 then (Cls.add x c, x, p, g)
	   else let f = father x in
		if f = -1 then aux (Cls.add x c) s1 p x g
		else let c1 = clause f in
		     let (c2,s2) = add c1 c s1 in
		     let g' = Cls.fold (fun y l -> if y = x then l else (read y, x) :: l) c1 g in 
		     if debug then begin
		       print_string "Clause père de "; 
		       print_int x; 
		       print_string ": ";
		       print_list (Cls.elements c1);
		       print_string "\nNouveau set: ";
		       print_list (Cls.elements s2);
		       print_newline() end;
		     aux c2 s2 (Proof.build (Cls.union c2 s2) c1 p) x0 g' in
  (* Renvoie la clause engendrée par le backtrack. *)
  let (c1,s) = add c Cls.empty Cls.empty in
  if debug then begin
    print_string "Set de départ: ";
    print_list (Cls.elements s);
    print_newline() end;
  aux c1 s (Proof.singleton c) 0 []
  (* Génère une preuve de résolution à partir d'une clause
     insatisfaite c, et donne en plus la valeur du potentiel
     point d'articulation. *)

  let prf c = Cls.fold (fun x d -> max (depth x) d) (clause c) 0

  let proof c = (fun (x, y, z, t) -> z) (calcul (clause c))
  (* Preuve de résolution à partir de la clause insatisfaite c. *)

  let graphe c = (fun (x, y, z, t) -> t) (calcul (clause c))
  (* Graphe des conflits à partir de la clause insatisfaite c. *)

  let backtrack c b =
    let (c1,x,_,_) = calcul (clause c) in
    let d = Cls.fold (fun y d -> if y <> x then max (depth y) d else d) c1 0 in
    (* On cherche la profondeur de backtrack maximale dans cette
       clause. *)
    add_clause (Cls.add x c1);
    (* On ajoute la clause ainsi créée. *)
    if debug then begin
      print_string "Clause engendrée pendant le backtrack: ";
      print_list (Cls.elements (Cls.add x c1));
      print_string "\nNouvelle profondeur de backtrack: ";
      print_int d;
      print_newline()
    end;
    if b then begin
      print_string "Clause engendrée pendant le backtrack: ";
      print_list (Cls.elements (Cls.add x c1));
      print_newline() end;
    d
(* Donne la profondeur de bactrack à laquelle remonter, et la valeur du point d'articulation. *)
end;;

module type Abstract =
sig
  exception Satisfiable
  type cls = int
  exception Unsatisfiable of cls
  type proof
  val var : int
  val nb_cls : unit -> int
  val lst : int list list
  val ord : (int * int) list
  val wlit : bool
  val graph : bool
  val aff : bool
  val smt : bool
  val heur : string
  val fix_depth : int -> unit
  val restore : int -> unit
  val read : int -> int
  val write : ?father:int -> int -> unit
  val write_father : int -> cls -> unit
  val depth : int -> int
  val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val fill : int list -> cls
  val iter : (int -> unit) -> cls -> unit
  val cls_make : int -> cls
  val cls_fold : (int -> 'a -> 'a) -> cls -> 'a -> 'a
  val add_cls : int list -> int
  val literals : cls -> int list
  val length : cls -> int
  val choose : cls -> int
  val is_singleton : cls -> int
  val backtrack : cls -> bool -> int
  val father : int -> cls
  val prf : cls -> int
  val proof : cls -> proof
  val graphe : cls -> (int * int) list
  val get : proof -> (int list * proof * proof)
  val size : proof -> int
end;;

module Make = (Core : Abstract);;
