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
  { LBKT }
| "]"	    
  { RBKT }
| "\\/"
  { OR }
| "/\\"
  { AND }
| "=>"
  { IMPLY }
| "~"
  { NOT }
| "=="
  { EQUAL }
| "!="
  { DIFF }
| ['\\'] ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
  { FUN (Lexing.lexeme lexbuf) }
| ['_''a'-'z' 'A'-'Z'] ['_''a'-'z' 'A'-'Z' '0'-'9']*
  { VAR (Lexing.lexeme lexbuf) }
| eof
  { EOF }
