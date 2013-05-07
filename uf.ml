(* Utiliser un champ mutable pour position d'un element dans le tableau *)

module type UfElt =
sig
  val nbr : int
end;;


module UfCore = functor (Elt : UfElt) ->
struct
  
  let arrayCls = Array.init Elt.nbr (fun i -> i)
  let print () = Array.iteri (fun i x -> if i = 0 then print_int x
      else begin print_string "; "; print_int x end) arrayCls


  let rec find i =
    if arrayCls.(i) = i then i
    else begin
      let j = find arrayCls.(i) in
        arrayCls.(i) <- j; j
    end

  let union i j =
    if arrayCls.(i) < arrayCls.(j) then
      arrayCls.(find j) <- arrayCls.(i)
    else
      arrayCls.(find i) <- arrayCls.(j)

end;;

module Test = UfCore (struct let nbr = 10 end);;

Test.print ();;
Test.union 5 4;;
Test.print ();;
Test.union 6 7;;
Test.print ();;
Test.union 5 7;;
Test.print ();;
Test.union 2 4;;
Test.print ();;
Test.union 1 4;;
Test.print ();;
Test.union 1 0;;
Test.print ();;
Test.find 7;;
Test.print ();;

