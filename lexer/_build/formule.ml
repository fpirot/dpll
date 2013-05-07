type var = string

type formule = Var of var | Or of formule * formule | And of formule * formule | Not of formule | Imply of formule * formule
