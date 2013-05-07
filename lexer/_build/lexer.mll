{
  open Parser
}

rule lexer = parse
| [' ' '\t' '\n']
  { lexer lexbuf }
| "("	    
  { LPAR }
| ")"	    
  { RPAR }
| "\\/"
  { OR }
| "/\\"
  { AND }
| "=>"
  { IMPLY }
| "~"
  { NOT }
| ['_''a'-'z' 'A'-'Z'] ['_''a'-'z' 'A'-'Z' '0'-'9']*
  { VAR (Lexing.lexeme lexbuf) }
| eof
  { EOF }
