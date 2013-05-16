%{
  open Formule
%}

%token EOF
%token LBKT RBKT
%token LPAR RPAR
%token EQ DF
%token <string> VAR FUN

%nonassoc NOT
%left EQ DF
%right FUN
%nonassoc LPAR RPAR

%start terms
%start pred
%type <Smt.terms> terms
%type <Smt.predicat> pred

%%

terms :
  | LPAR terms RPAR {
    $2 }
  | FUN terms terms {
    Fun ($2, [$3]) }
  | VAR, {
    Var ($1) }

pred :
  | LBKT terms RBKT {
    ($2) }
  | pred EQ pred {
    Equal ($1, $3) }
  | pred DF pred {
    Diff ($1, $3) }

%%  
