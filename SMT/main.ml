open Type;;
open Smt;;

type 'a alloc = {add : 'a -> int; find : int -> 'a}

let table =

  let compt = ref (-1)
  and ptovTable = Hashtbl.create 257
  and vtopTable = Hashtbl.create 257 in
  let var p = try Hashtbl.find ptovTable p with Not_found -> incr compt; Hashtbl.add ptovTable p !compt; !compt
  and pred n x = let _ = try Hashtbl.find vtopTable n with Not_found -> Hashtbl.add vtopTable n x; x in () in
  {add = (fun x -> pred (var x) x; !compt);
   find = (fun n -> Hashtbl.find vtopTable n)};;

let rec convert_formule = function
  | POr(a, b) -> Or(convert_formule a, convert_formule b)
  | PAnd(a, b) -> And(convert_formule a, convert_formule b)
  | PImply(a, b) -> Imply(convert_formule a, convert_formule b)
  | PNot(a) -> Not(convert_formule a)
  | PPred(p) -> Var (table.add p)
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
  | Var n -> Printf.fprintf file "x%d" n
  | Pred p -> failwith "Bad input format in SMT"
and print_terms = function
  | Fun(a, b) -> print_string ("Fun ("^a^" ("); List.iter (fun x -> print_terms x; print_string " ") b; print_string ")"
  | Cst(v) -> print_string v;;

let main =
  let channel = open_in "test" in
  let lexbuf = Lexing.from_channel channel in
  let file = open_out "../Test/smt.cnf" in
  print_formule file (convert_formule (Parser.pform Lexer.lexer lexbuf));
  close_out file;;
