open Type;;

type 'a alloc = {write : 'a -> int; read : int -> 'a; var : 'a -> int}

let table =

  let compt = ref 0
  and ptovTable = Hashtbl.create 257
  and vtopTable = Hashtbl.create 257 in
  let var p = try Hashtbl.find ptovTable p with Not_found -> incr compt; Hashtbl.add ptovTable p !compt; !compt
  and pred n x = try Hashtbl.find vtopTable n with Not_found -> Hashtbl.add vtopTable n x; x
  and find p = try Hashtbl.find ptovTable p with Not_found -> 0 in
  {write = (fun x -> let n = var x in let _ = pred n x in n);
   read = (fun n -> Hashtbl.find vtopTable n);
   var = find};;

let rec convert_formule = function
  | POr(a, b) -> Or(convert_formule a, convert_formule b)
  | PAnd(a, b) -> And(convert_formule a, convert_formule b)
  | PImply(a, b) -> Imply(convert_formule a, convert_formule b)
  | PNot(a) -> Not(convert_formule a)
  | PPred(p) -> begin match p with
      | PEqual (a, b) -> Var (table.write (convert_predicat p))
      | PDiff (a, b) -> convert_formule (PNot(PPred(PEqual (a, b))))
(*
      | PEqual (a, b) -> let xdiff = table.var (convert_predicat (PDiff (a, b))) in
			 let x = if xdiff = 0 then table.write (convert_predicat p) else -xdiff in Var x
      | PDiff (a, b) ->  let xeq = table.var (convert_predicat (PEqual (a, b))) in
			 let x = if xeq = 0 then - table.write (convert_predicat p) else -xeq in Var x
*)  end
and convert_predicat = function
  | PEqual(a, b) -> Equal(convert_terms a, convert_terms b)
  | PDiff(a, b) -> Diff(convert_terms a, convert_terms b)
and convert_terms = function
  | PFun(a, b) -> begin match b with
      | PList(c, d) -> Fun(a, convert_list b)
      | _ -> Fun(a, [convert_terms b]) end
  | PVar(v) -> Cst(v)
  |_ -> failwith "Convert"
and convert_list = function
  |PList(a, b) -> (convert_terms a)::(convert_list b)
  |PVar(v) -> [Cst(v)]
  |_ -> failwith "Convert";;

let rec print_formule file = function
  | Or(a, b) -> Printf.fprintf file "("; print_formule file a; Printf.fprintf file " \\/ "; print_formule file b; Printf.fprintf file ")"
  | And(a, b) -> Printf.fprintf file "("; print_formule file a; Printf.fprintf file " /\\ "; print_formule file b; Printf.fprintf file ")"
  | Imply(a, b) -> Printf.fprintf file "("; print_formule file a; Printf.fprintf file " => "; print_formule file b; Printf.fprintf file ")"
  | Not a -> Printf.fprintf file "~("; print_formule file a; Printf.fprintf file ")"
  | Var n -> if n > 0 then Printf.fprintf file "x%d" n else Printf.fprintf file "~x%d" (-n)
  | Pred p -> failwith "Bad input format in SMT"
and print_terms = function
  | Fun(a, b) -> print_string (a ^ " ( "); List.iter (fun x -> print_terms x; print_string " ") b; print_string ")"
  | Cst(v) -> print_string v;;

let print_pred p b = match (p,b) with
  | (Equal (a, b), true)
  | (Diff (a, b), false) -> print_terms a; print_string " == "; print_terms b; print_newline()
  | (Equal (a, b), false)
  | (Diff (a, b), true) -> print_terms a; print_string " != "; print_terms b; print_newline()

let main channel =
  let lexbuf = Lexing.from_channel channel in
  let file = open_out "Test/smt.cnf" in
  print_formule file (convert_formule (Parser.pform Lexer.lexer lexbuf));
  close_out file;;
