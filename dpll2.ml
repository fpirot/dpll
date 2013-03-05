let rec valuation n =
  let rec aux l = function
    | 0 -> l
    | n -> let v = Assig.read n in
	   if v = 0 then aux (n :: l) (n-1)
	   else aux (v :: l) (n-1)
  in aux [] n;;

let dpll env = 
  let rec aux env =
    if Op.is_empty env then ()
    else begin
    let (x, (ltrue, envtrue), (lfalse, envfalse)) = Op.split env in
    try (
      Assig.write x x; 
      let env' = Op.propagation envtrue in
      aux env')
    with Unsatisfiable -> (
      Assig.write x (-x);
      let env' = Op.propagation envfalse in
      aux env')
    end;
  in aux env; valuation Assig.nbr;;
  
