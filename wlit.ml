open Dpll;;

type 'a wclause = {lit : 'a Clause.t; wlit : 'a * 'a};;
(* lit contient l'ensemble des litéraux de la clause, wlit contient les deux variables surveillées dans la clause (dont la valuation est donc indéterminée). *)

