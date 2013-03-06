(* Ce fichier regroupe et assemble tout les autres fichiers.
Pour editer ce fichier, copier dans le module correspondant 
le corps du fichier associer. *)

(* Core *)

module Core = struct

module Load =
		struct

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


		let init c channel =
			let rec iter list lstC lstV = function
				  | 0 -> Scanf.bscanf channel " %d "
				      (fun x -> if x = 0 then (lstV :: lstC, list)
				              else iter (insert (abs x) list) lstC (x :: lstV) 0)
				  | n -> Scanf.bscanf channel " %d "
				      (fun x -> if x = 0 then iter list (lstV :: lstC) [] (n - 1)
				              else iter (insert (abs x) list) lstC (x :: lstV) n) in
				iter [] [] [] (c - 1)


		let load channel =
			Scanf.bscanf channel "p cnf %d %d" (fun v c -> (v, c, init c channel))

  end;;

module Core =
  struct
    
    type order = (int * int) list
    
    let debug = false
    let (var, cls, (lst, ord)) = Load.load (Scanf.Scanning.open_in "test")
    
    let assigArray = Array.create var 0
    let read n = assigArray.(n - 1)
    let write n x = assigArray.(n - 1) <- x;
      if debug then begin
        print_string "Assignment: ";
        Array.iter (fun x -> print_int x; print_char ' ') assigArray;
        print_string "\n\n" end
    
    let hd l = snd (List.hd l)
    let tl l = List.tl l
    let update l = 
      if debug then begin
        print_string "Order: ";
        List.iter (fun x -> print_int (snd x); print_char ' ') l;
        print_string "\n\n" end; l
    
    let fold = List.fold_right
    
  end;;

module type Abstract =
  sig
    type order
    val var : int
    val cls : int
    val lst : int list list
    val ord : order
    val read : int -> int
    val write : int -> int -> unit
    val hd : order -> int
    val tl : order -> order
    val update : order -> order
    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  end;;

module Make = (Core : Abstract);;

end;;



(* Clause *)


module Clause = struct

module type ClauseElt =
  sig
    val cls : int
    val lst : int list list
    val read : int -> int
    val write : int -> int -> unit
    val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  end;;

module ClauseCore = functor (Elt : ClauseElt) ->
  struct
    module Cls = Set.Make
      (struct
        type t = int
        let compare x y = compare (abs x) (abs y)
      end)
    (* Les clauses sont des ensembles d'entiers (+x pour le litéral vrai de la variable x, -x pour sa négation), 
       avec la relation de comparaison sur les valeurs absolues (entre nom de variable). *)
    
    module St = Set.Make
      (struct
        type t = int
        let compare = compare
      end)
    (* Une structure d'ensemble d'entiers avec la comparaison habituelle. *)
    
    module Mp = Map.Make
      (struct
        type t = int
        let compare = compare
      end)
    (* On gère une table d'association qui à chaque litéral associe l'ensemble des indices de clauses qui le contiennent. *)
    
    let clauseArray = Array.make Elt.cls Cls.empty
    (* On référencie l'ensemble des clauses dans un tableau, afin de stocker des indices dans nos structures de données plutôt que des clauses. *)
    
    let compt = ref (-1)
    (* L'indice en cours dans le tableau. *)
    
    let debug = true
    let print_list l=
      let rec print = function
        |[] -> print_string "]"
        |[a] -> print_int a; print_string "]"
        |a::l -> print_int a; print_string "; "; print l in
      print_string "["; print l;
    
    type cls = int
    type set = St.t
    type map = St.t Mp.t
    
    let empty = Mp.empty
    (* Table d'association vide. *)
    
    let fill l =
      incr compt;
      clauseArray.(!compt) <- Elt.fold (fun x s -> Cls.add x s) l Cls.empty;
      if debug then begin
        print_string "clauseArray status:\n";
          Array.iteri (fun i x ->
              print_int i;
              print_string ": ";
              print_list (Cls.elements x);
              print_newline())
            clauseArray;
          print_newline()
      end;
      !compt
    (* Renvoie dans la case du tableau en cours la clause représentée par sa liste d'entiers l. *)
    
    let add id map =
      Cls.fold (fun x m -> let s = try Mp.find x m with _ -> St.empty in
        Mp.add x (St.add id s) m) clauseArray.(id) map
    (* Ajoute la clause d'indice id dans la table d'association. *)
    
    let reset () =
      Array.fill clauseArray 0 (Elt.cls - 1) Cls.empty;
      if debug then begin
        print_string "clauseArray status:\n";
          Array.iteri (fun i x ->
              print_int i;
              print_string ": ";
              print_list (Cls.elements x);
              print_newline())
            clauseArray;
          print_newline()
      end;
      compt := -1
    (* Réinitialise le tableau de clauses. *)
    
    let create lst =
      reset ();
      Elt.fold (fun l m -> add (fill l) m) lst Mp.empty
    
    let is_empty = Mp.is_empty
    (* Teste si la table d'association est vide. *)

    let are_sat id = Cls.fold (fun x c -> if Elt.read x = 0 then c+1 else c) clauseArray.(id) 0
    (* Renvoie le nombre de litéraux dont on ne connaît pas encore l'assignation dans la clause. *)

    let mem = Mp.mem
    (* Indique si une variable est présente dans l'ensemble des clauses. *)

    let literals id = Cls.elements clauseArray.(id)
    (* Donnes les elements d'une clause *)

    let remove id map =
      Cls.fold (fun x m -> Mp.add x (St.remove id (Mp.find x m)) m) clauseArray.(id) map
    (* Supprime une clause de la map *)

    let bindings m = let lst = Mp.bindings m in
      List.map (fun (k, s) ->
        (k, List.map (fun id -> Cls.elements clauseArray.(id)) (St.elements s))) lst
    (* Affichage des éléments de la table d'association sous forme de liste. *)
    
    let elements id = Cls.elements clauseArray.(id)
    (* Affichage des éléments d'une clause sous forme de liste. *)

    (*let extract x map =
      let s = Mp.find x map
      and m = remove x map in (St.elements s, m)*)
    
    let extract x map = 
      let s = Mp.find x map in
      let m = St.fold (fun id m -> remove id m) s map in
        if debug then begin
          print_string "Extraction:\n";
          List.iter (fun x ->
		          print_int x;
		          print_string ": ";
		          print_list (Cls.elements clauseArray.(x));
		          print_newline())
            (St.elements s);
          print_newline ();
          print_string "New map:\n";
          List.iter (fun (x, lst) ->
              if lst <> [] then begin
				          print_int x;
				          print_string ": ";
				          List.iter (fun l -> print_list l; print_char ' ') lst;
				          print_newline() end)
		          (bindings (Mp.remove x m));
          print_newline();
          (*print_string "clauseArray status:\n";
          Array.iteri (fun i x ->
              print_int i;
              print_string ": ";
              print_list (Cls.elements x);
              print_newline())
            clauseArray;
          print_newline()*)
        end;
        (St.elements s, Mp.remove x m)
    
    let choose id = Cls.choose clauseArray.(id)
    
    let find x m = St.elements (Mp.find x m)

    (* Renvoie la liste de toutes les clauses attachées à un litéral, et la table d'association privée de ces clauses et de la négation du litéral (lorsque l'on donne à une variable une assignation particulière). *)
    
  end;;


module type ClauseAbstract = functor (Elt : ClauseElt) ->
  sig
    type map
    type cls
    val empty : map
    val fill : int list -> cls
    val add : cls -> map -> map
    val create : int list list -> map
    val reset : unit -> unit
    val is_empty : map -> bool
    val are_sat : cls -> int
    val mem : int -> map -> bool
    val literals : cls -> int list
    val remove : cls -> map -> map
    val bindings : map -> (int * int list list) list
    val elements : cls -> int list
    val extract : int -> map -> cls list * map
    val choose : cls -> int
    val find : int -> map -> cls list
  end;;

module Make = (ClauseCore : ClauseAbstract);;

end;;



(* Oper *)

module Oper = struct

module type CoreElt =
  sig
    type order
    val lst : int list list
    val ord : order
    val hd : order -> int
    val tl : order -> order
    val read : int -> int
    val write : int -> int -> unit
  end;;


module type OpElt =
(* Module qui référencie l'ensemble des clauses du problème. *)
  sig
    type cls
    type map
    val empty : map
    val create : int list list -> map
    val is_empty : map -> bool
    val are_sat : cls -> int
    val find : int -> map -> cls list
    val choose : cls -> int
    val bindings : map -> (int * int list list) list
    val extract : int -> map -> cls list * map 
  end ;;



module OpCore = functor (Elt : OpElt) -> functor (Cor : CoreElt) ->
  struct
    
    exception Satisfiable;;
    exception Unsatisfiable;;
    
    type env = {clause: Elt.map; order: Cor.order}
    type cls = Elt.cls
    type map = Elt.map

    module St = Set.Make (
      struct
      	type t = int
      	let compare = compare
      end)

    let create () = 
      {clause = Elt.create Cor.lst; order = Cor.ord}

(* Extrait une variable selon l'ordre *)
    let split env =
      let k = Cor.hd env.order in
      let (ltrue, mtrue) = Elt.extract k env.clause
      and (lfalse, mfalse) = Elt.extract (-k) env.clause in
        (k, (ltrue, {clause = mtrue; order = Cor.tl env.order}),
	      (lfalse, {clause = mfalse; order = Cor.tl env.order}))
    
    let is_empty env = Elt.is_empty env.clause

    let select lc setv = 	
      List.fold_right (fun c s -> let n = Elt.are_sat c in
				  if n = 0 then raise Unsatisfiable
				  else if n = 1 then 
				    let x = Elt.choose c in St.add x s
				  else s) lc setv

    let rec propagation env lc =
      let rec aux env lc setv =
				let setv' = select lc setv in
				if St.is_empty setv' then env
				else begin
					let x = St.choose setv' in
					Cor.write (abs x) x;
					let (_, m) = Elt.extract x env.clause in
					let lc' = Elt.find (-x) env.clause in
					aux {clause = m; order = Cor.tl env.order} lc' setv'
				end
      in aux env lc St.empty
    
    let bindings env = Elt.bindings env.clause

  end;;


module type OpAbstract = functor (Elt : OpElt) -> functor (Cor : CoreElt) ->
  sig
    exception Satisfiable
    exception Unsatisfiable
    type env
    type cls
    type map
    val create : unit -> env
    val split : env -> (int * (cls list * env) * (cls list * env))
    val is_empty : env -> bool
    val propagation : env -> cls list -> env
    val bindings : env -> (int * int list list) list
  end;;


module Make = (OpCore : OpAbstract);;

end;;


(* Test *)

module Core = Core.Make;;
module Clause = Clause.Make(Core);;
module Oper = Oper.Make(Clause)(Core);;

let env = Oper.create ();;
Oper.bindings env;;
let (k, (lstT, envT), (lstF, envF)) = Oper.split env in
  (k, Oper.bindings envF);;
