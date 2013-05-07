type token =
  | EOF
  | LPAR
  | RPAR
  | OR
  | AND
  | IMPLY
  | NOT
  | VAR of (string)

val form :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Formule.formule
