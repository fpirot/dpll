type pterms = PFun of string * pterms | PVar of string | PList of pterms * pterms;;
type ppredicat = PEqual of pterms * pterms | PDiff of pterms * pterms;;
type pformule = PPred of ppredicat | POr of pformule * pformule | PAnd of pformule * pformule
              | PNot of pformule | PImply of pformule * pformule;;

type terms = Fun of string * terms list | Cst of string
type predicat = Equal of terms * terms | Diff of terms * terms;;
type formule = Pred of predicat | Or of formule * formule | And of formule * formule
             | Not of formule | Imply of formule * formule | Var of int;;
