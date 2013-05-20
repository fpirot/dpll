%{
  open Smt
%}

%token EOF
%token LPAR RPAR LBKT RBKT
%token OR AND IMPLY NOT
%token EQUAL DIFF
%token <string> VAR VARV FUN

%nonassoc NOT
%left OR AND
%right IMPLY
%nonassoc LPAR RPAR LBKT RBKT

%start form
%type <Smt.formule> form
%start terms
%type <Smt.terms> terms
%start pred
%type <Smt.predicat> pred

%%

terms :
  | LPAR terms RPAR {
    $2 }
  | FUN terms {
    Fun ($1, [$2]) }
  | VARV {
    Var ($1); }
  | VAR {
    Var ($1) }

pred :
  | LPAR pred RPAR {
    $2 }
  | terms EQUAL terms {
    Equal ($1,$3) }
  | terms DIFF terms {
    Diff ($1,$3) }

form :
  | LPAR form RPAR {
    $2 }
  | form OR form {
    Or ($1,$3) }
  | form AND form {
    And ($1,$3) }
  | form IMPLY form {
    Imply ($1,$3) }
  | NOT form {
    Not ($2) }
  | LBKT pred RBKT {
    Pred ($2) }

%%  
