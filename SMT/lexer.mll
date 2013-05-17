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
| "["	    
  { LPAR }
| "]"	    
  { RPAR }
| "\\"
  { FUN }
| "=="
  { EQ }
| "!="
  { DF }
| ['_''a'-'z' 'A'-'Z'] ['_''a'-'z' 'A'-'Z' '0'-'9']*
  { VAR (Lexing.lexeme lexbuf) }
| eof
  { EOF }
