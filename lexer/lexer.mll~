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
| "\092/"
  { OR }
| "/\092"
  { AND }
| "=>"
  { IMPLY }
| "~"
  { NOT }
| ['_''a'-'z' 'A'-'Z'] ['_''a'-'z' 'A'-'Z' '0'-'9']*
  { VAR (Lexing.lexeme lexbuf) }
| eof
  { EOF }
