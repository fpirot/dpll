%{
  open Formule
%}

%token EOF
%token LPAR RPAR
%token OR AND IMPLY NOT
%token <string> VAR

%nonassoc NOT
%left OR AND
%right IMPLY
%nonassoc LPAR RPAR

%start form
%type <Formule.formule> form

%%

form :
  | LPAR form RPAR {
    $2 }
  | form OR form {
    Or ($1,$3) }
  | form AND form {
    And ($1,$3) }
  | NOT form {
    Not ($2) }
  | VAR {
    Var ($1) }

%%  
