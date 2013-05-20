open Type;;
open Smt;;

type 'a alloc = {add : 'a -> int; find : int -> 'a}

let table =

  let compt = ref (-1)
  and ptovTable = Hashtbl.create 257
  and vtopTable = Hashtbl.create 257 in
    let ptov x = try Hashtbl.find ptovTable x with Not_found -> incr compt; Hashtbl.add ptovTable x !compt; !compt
    and vtop n x = try Hashtbl.find vtopTable n with Not_found -> Hashtbl.add vtopTable n x in
    
{add = (fun x -> vtop (ptov x) x; !compt);
find = (fun n -> Hashtbl.find vtopTable n)};;

let rec convert_formule = function
  | POr(a, b) -> Or(convert_formule a, convert_formule b)
  | PAnd(a, b) -> And(convert_formule a, convert_formule b)
  | PImply(a, b) -> Imply(convert_formule a, convert_formule b)
  | PNot(a) -> Not(convert_formule a)
  | PPred(p) -> Pred(convert_predicat p)
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

let rec print_formule = function
  | Or(a, b) -> print_string "Or ("; print_formule a; print_string ", "; print_formule b; print_string ")"
  | And(a, b) -> print_string "And ("; print_formule a; print_string ", "; print_formule b; print_string ")"
  | Imply(a, b) -> print_string "Imply ("; print_formule a; print_string ", "; print_formule b; print_string ")"
  | Not(a) -> print_string "Not ("; print_formule a; print_string ")"
  | Pred(p) -> print_string "Pred ("; print_predicat p; print_string ")"
and print_predicat = function
  | Equal(a, b) -> print_terms a; print_string " == "; print_terms b; print_string ")"
  | Diff(a, b) -> print_terms a; print_string " != "; print_terms b; print_string ")"
and print_terms = function
  | Fun(a, b) -> print_string ("Fun ("^a^" ("); List.iter (fun x -> print_terms x; print_string " ") b; print_string ")"
  | Cst(v) -> print_string v;;

let main =
  let channel = open_in "test" in
  let lexbuf = Lexing.from_channel channel in
    print_formule (convert_formule (Parser.pform Lexer.lexer lexbuf));
    print_newline();;
