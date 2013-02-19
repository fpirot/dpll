exception Satisfiable;;
exception Unsatisfiable;;

module Data = struct
  type t = (int * int)
  let compare = compare
end;;
(* Data représente une variable et la fréquence d'apparition qui lui est associée. *)

module Clause = Map.Make (Data);;
(* Une clause est représentée sous forme d'un arbre de variables. *)


(* Input/Output *)

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
                  (m, y) :: (m', y') :: l';;
(* Insertion d'une variable dans une liste de Datas, en assurant un tri au passage. *)


let init v c channel =
  let rec iter list lstC lstV = function
      | 0 -> Scanf.bscanf channel " %d "
          (fun x -> if x = 0 then (lstV :: lstC, list)
                  else iter (insert (abs x) list) lstC (x :: lstV) 0)
      | n -> Scanf.bscanf channel " %d "
          (fun x -> if x = 0 then iter list (lstV :: lstC) [] (n - 1)
                  else iter (insert (abs x) list) lstC (x :: lstV) n) in
    iter [] [] [] (c - 1);;
(* Fonction de récupération des clauses dans une liste à partir d'un chaîne de caractères. *)


let load channel =
  Scanf.bscanf channel "p cnf %d %d" (fun v c -> init v c channel);;


(* Bucket *)

type 'a env = {assig: int list; bucket: 'a; order: (int * int) list};;
(* Un environnement contient une liste de valuations de variables, des seaux de type non défini, et un ordre... *)


let rec key lstV = function
    | [] -> failwith "Key"
    | a :: l -> if List.exists (fun x -> abs x = snd a) lstV
        then a else key lstV l;;
(* Renvoie le couple (clé, objet) pour le premier objet de lstV qui apparaisse dans une liste de couple (clé, objet). *)


let create (lstC, list) =
  let e = Clause.empty in
    {assig = [];
      bucket = List.fold_left (fun m x -> let k = key x list in
                Clause.add k (x :: (try Clause.find k m with _ -> [])) m) e lstC;
      order = list};;


let split env =
  let k = List.hd env.order in
    let (m1, m2) = Clause.partition (fun x _ -> x = k) env.bucket in
      ((snd k, Clause.find k m1),
        {assig = env.assig;
          bucket = m2;
          order = List.tl env.order});;


let assignment (x, lstC) =

  let rec assigT lst = function
      | [] -> (true, lst)
      | y :: l -> if x + y = 0 then raise Satisfiable else
          if x - y = 0 then assigT lst l
          else assigT (y :: lst) l
  and assigF lst = function
      | [] -> (false, lst)
      | y :: l -> if x - y = 0 then raise Satisfiable else
          if x + y = 0 then assigF lst l
          else assigF (y :: lst) l in

    let rec assigC optT optF = function
        | [] -> ((x, optT), (- x, optF))
        | c :: l -> match (optT, optF) with
              | None, None -> failwith "Assignment"
              | Some lstT, None -> begin match
                    (try assigF [] c with Satisfiable -> true, []) with
                      | true, _ -> assigC optT optF l
                      | false, [] -> raise Unsatisfiable
                      | false, lstV -> assigC (Some (lstV :: lstT)) None l
                  end
              | None, Some lstF -> begin match
                    (try assigT [] c with Satisfiable -> false, []) with
                      | false, _ -> assigC optT optF l
                      | true, [] -> raise Unsatisfiable
                      | true, lstV -> assigC None (Some (lstV :: lstF)) l
                  end
              | Some lstT, Some lstF -> try (begin match assigV [] c with
                          | false, [] -> assigC None  (Some lstF) l
                          | true, [] -> assigC (Some lstT) None l
                          | false, lstV -> assigC (Some (lstV :: lstT)) optF l
                          | true, lstV -> assigC optT (Some (lstV :: lstF)) l
                      end) with Satisfiable -> assigC optT optF l
    and assigV lst = function
        | [] -> failwith "Assignment"
        | y :: l -> if x + y = 0 then assigF lst l else
            if x - y = 0 then assigT lst l
            else assigV (y :: lst) l in

      assigC (Some []) (Some []) lstC;;


let merge (x, a) env = match a with
    | None -> raise Unsatisfiable
    | Some lst ->
        {assig = x :: env.assig;
          bucket = List.fold_left (fun m x -> let k = key x env.order in
                    Clause.add k (x :: (try Clause.find k m with _ -> [])) m)
          env.bucket lst;
          order = env.order};;


let rec verify list m = match Clause.is_empty m with
    | true -> true
    | false -> let (k, lst) = Clause.max_binding m in
          (List.for_all (fun l ->
                    (List.exists (fun x ->
                              List.exists (fun y -> y = x) list) l)) lst)
          && verify list (Clause.remove k m);;


let algoDPLL channel =
  let e = create (load channel)
  and assig = ref [] in
    let rec main env = match Clause.is_empty env.bucket with
        | true -> assig := env.assig; raise Satisfiable;
        | _ -> let (a, env) = split env in let (a, b) = assignment a in
                ((try main (merge a env) with Unsatisfiable -> ());
                  (try main (merge b env) with Unsatisfiable -> ())) in
      try (main e; print_string "Unsatisfiable ") with
        Satisfiable -> (List.iter (fun x -> print_int x; print_char ' ') (List.sort (fun x y -> compare (abs x) (abs y)) !assig);
              if verify (!assig) e.bucket
              then print_string " Verified" else print_string " Error");;


(* Test *)
(*
load (Scanf.Scanning.open_in "Test/ex1.cnf");;

let (lstC, list) = load (Scanf.Scanning.open_in "Test/ex1.cnf") in
  List.map (fun x -> key x list) lstC;;

let (lstC, list) = load (Scanf.Scanning.open_in "Test/ex1.cnf") in
  Clause.bindings (create (lstC, list)).bucket;;

let (lstC, list) = load (Scanf.Scanning.open_in "Test/ex1.cnf") in
  split (create (lstC, list));;

let (lstC, list) = load (Scanf.Scanning.open_in "Test/ex1.cnf") in
  assignment (fst (split (create (lstC, list))));;

let (lstC, list) = load (Scanf.Scanning.open_in "Test/ex1.cnf") in
  let (a, env) = split (create (lstC, list)) in
    let (a, b) = (merge (fst (assignment a)) env, merge (snd (assignment a)) env) in
      (a, Clause.bindings a.bucket, b, Clause.bindings b.bucket);;

algoDPLL (Scanf.Scanning.open_in "Test/ex1.cnf");;
*)
