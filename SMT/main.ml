open Smt;;

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
  | Fun(a, b) -> print_string ("Fun ("^a^", "); List.iter (fun x -> print_terms x) b
  | Var(v) -> print_string v;;
  

let main =
  let channel = open_in "test" in
  let lexbuf = Lexing.from_channel channel in
    print_formule (Parser.form Lexer.lexer lexbuf);
    print_newline();;
