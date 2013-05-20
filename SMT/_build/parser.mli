type token =
  | EOF
  | LPAR
  | RPAR
  | LBKT
  | RBKT
  | OR
  | AND
  | IMPLY
  | NOT
  | EQUAL
  | DIFF
  | VAR of (string)
  | FUN of (string)

val pform :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Type.pformule
val pterms :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Type.pterms
val ppred :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Type.ppredicat
