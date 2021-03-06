%{
  open Type
%}

%token EOF
%token LPAR RPAR LBKT RBKT
%token OR AND IMPLY NOT
%token EQUAL DIFF
%token <string> VAR FUN

%nonassoc NOT
%left OR AND
%right IMPLY
%nonassoc LPAR RPAR LBKT RBKT

%start pform
%type <Type.pformule> pform
%start pterms
%type <Type.pterms> pterms
%start ppred
%type <Type.ppredicat> ppred

%%

pterms :
  | LPAR pterms RPAR {
    $2 }
  | FUN LPAR pterms RPAR {
    PFun ($1, $3) }
  | VAR pterms {
    PList (PVar ($1), $2) }
  | VAR {
    PVar ($1) }

ppred :
  | LPAR ppred RPAR {
    $2 }
  | pterms EQUAL pterms {
    PEqual ($1,$3) }
  | pterms DIFF pterms {
    PDiff ($1,$3) }

pform :
  | LPAR pform RPAR {
    $2 }
  | pform OR pform {
    POr ($1,$3) }
  | pform AND pform {
    PAnd ($1,$3) }
  | pform IMPLY pform {
    PImply ($1,$3) }
  | NOT pform {
    PNot ($2) }
  | LBKT ppred RBKT {
    PPred ($2) }

%%  
