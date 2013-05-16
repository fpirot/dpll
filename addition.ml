module Print =
struct

  let nbr = 32

  let init = fun n ->
    let bitArray = Array.init nbr (fun i -> n land (1 lsl i) > 0) in
      (fun m -> bitArray.(m))

  let sum channel =
    Printf.fprintf channel "-%d -%d -%d 0\n" 1 (nbr + 1) (3 * nbr + 1);
    Printf.fprintf channel "-%d %d %d 0\n" 1 (nbr + 1) (3 * nbr + 1);
    Printf.fprintf channel "%d -%d %d 0\n" 1 (nbr + 1) (3 * nbr + 1);
    Printf.fprintf channel "%d %d -%d 0\n" 1 (nbr + 1) (3 * nbr + 1);
    for k = 1 to nbr - 1 do
      let a = k + 1
      and b = k + nbr + 1
      and r = (k + nbr - 1) mod nbr + 2 * nbr + 1
      and s = k + 3 * nbr + 1 in
  	    Printf.fprintf channel "-%d -%d -%d %d 0\n" a b r s;
  	    Printf.fprintf channel "-%d -%d %d -%d 0\n" a b r s;
  	    Printf.fprintf channel "-%d %d -%d -%d 0\n" a b r s;
  	    Printf.fprintf channel "%d -%d -%d -%d 0\n" a b r s;
  	    Printf.fprintf channel "-%d %d %d %d 0\n" a b r s;
  	    Printf.fprintf channel "%d -%d %d %d 0\n" a b r s;
  	    Printf.fprintf channel "%d %d -%d %d 0\n" a b r s;
  	    Printf.fprintf channel "%d %d %d -%d 0\n" a b r s;
    done

  let carry channel =
    Printf.fprintf channel "-%d -%d %d 0\n" 1 (nbr + 1) (2 * nbr + 1);
    Printf.fprintf channel "%d -%d 0\n" (nbr + 1) (2 * nbr + 1);
    Printf.fprintf channel "%d -%d 0\n" 1 (2 * nbr + 1);
    for k = 1 to nbr - 1 do
      let a = k + 1
      and b = k + nbr + 1
      and r = k + 2 * nbr
      and s = k + 2 * nbr + 1 in
        Printf.fprintf channel "-%d -%d %d 0\n" a b s;
        Printf.fprintf channel "-%d -%d %d 0\n" a r s;
        Printf.fprintf channel "-%d -%d %d 0\n" b r s;
        Printf.fprintf channel "%d %d -%d 0\n" a b s;
        Printf.fprintf channel "%d %d -%d 0\n" a r s;
        Printf.fprintf channel "%d %d -%d 0\n" b r s;
    done

  let add channel x y =
    let x = init x
    and y = init y in
      Printf.fprintf channel "p cnf %d %d\n" (nbr * 4) ((nbr - 1) * 16 + 9);
      for k = 0 to nbr - 1 do
        Printf.fprintf channel "%d 0\n" (if x k then k + 1 else -(k + 1));
      done; 
      for k = 0 to nbr - 1 do
        Printf.fprintf channel "%d 0\n" (if y k then k + nbr + 1 else -(k + nbr + 1));
      done;
      sum channel;
      carry channel;
      

end;;


(* Tests *)


let add x y =

  let channel = open_out "add" in
	  Print.add channel x y;
	  close_out channel;
  let _ = Sys.command "minisat add result"
  and channel = Scanf.Scanning.open_in "result" in
    let _ = Scanf.bscanf channel "SAT" (fun _ -> ()) in
      for k = 1 to 96 do
        Scanf.bscanf channel " %d" (fun x -> ())
      done;
      let result = ref 0 in
        for k = 0 to 31 do
          Scanf.bscanf channel " %d" (fun x -> if x > 0 then result := !result + (1 lsl k))
        done;
        Scanf.Scanning.close_in channel;   
  !result;;

let verif nbr =
  let rec auxi bl = function
    |0 -> bl
    |n -> let a = Random.int max_int
      and b = Random.int max_int in
        bl && auxi (add a b = a + b) (n - 1) in
  Random.self_init();
  auxi true nbr;;

verif 256;;

