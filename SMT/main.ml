open Smt;;

let rec print_formule = function
  | POr(a, b) -> print_string "Or ("; print_formule a; print_string ", "; print_formule b; print_string ")"
  | PAnd(a, b) -> print_string "And ("; print_formule a; print_string ", "; print_formule b; print_string ")"
  | PImply(a, b) -> print_string "Imply ("; print_formule a; print_string ", "; print_formule b; print_string ")"
  | PNot(a) -> print_string "Not ("; print_formule a; print_string ")"
  | PPred(p) -> print_string "Pred ("; print_predicat p; print_string ")"
and print_predicat = function
  | PEqual(a, b) -> print_terms a; print_string " == "; print_terms b; print_string ")"
  | PDiff(a, b) -> print_terms a; print_string " != "; print_terms b; print_string ")"
and print_terms = function
  | PList(a, b) -> print_terms a; print_string ", "; print_terms b
  | PFun(a, b) -> print_string ("Fun ("^a^", "); print_terms b
  | PVar(v) -> print_string v;;
  

let main =
  let channel = open_in "test" in
  let lexbuf = Lexing.from_channel channel in
    print_formule (Parser.pform Lexer.lexer lexbuf);
    print_newline();;
