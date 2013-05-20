module Print = 
struct

  let head channel =
    Printf.fprintf channel
      "\092documentclass{article}

\092usepackage{mathpartir}
\092usepackage[utf8]{inputenc}

\092newcommand{\092non}[1]{\092overline{#1}}
\092newcommand{\092varv}[1]{x_{#1}}
\092newcommand{\092varf}[1]{\092non{\092varv{#1}}}
\092newcommand{\092cl}[1]{\092mathtt{C_{#1}}:~}
\092newcommand{\092preuve}[1]{\092mathtt{\092Pi_{#1}}}

\092begin{document}"

  let literals channel x =
    if x > 0 then
      Printf.fprintf channel "\092varv{%d}" (abs x)
    else
      Printf.fprintf channel "\092varf{%d}" (abs x)
	
  let clause channel cls f_id f_elt =
    let lst = f_elt cls in
    Printf.fprintf channel "\n\092cl{%d} " (f_id cls);
    literals channel (List.hd lst);
    List.iter (fun x -> Printf.fprintf channel " \092lor "; literals channel x) (List.tl lst)
      
  let proof channel prf f_get f_size f_id f_elt =

    let f_decomp prf =
      let (l, a, b) = f_get prf in
      let (l', a', b') = f_get b in
      (l, a, l') in

    let rec iter p i =
      if f_size p > 5 then begin
	      let (c, p', c') = f_decomp p in
	      iter p' (i + 1);
	      Printf.fprintf channel "\092begin{mathpar}\n\092preuve{%d}:~\092inferrule{\n\092preuve{%d}\n\092and" i (i + 1);
	      clause channel c' f_id f_elt;
	      Printf.fprintf channel "\n}\n{";
	      clause channel c f_id f_elt;
	      Printf.fprintf channel "\n}\n\092end{mathpar}\n" end
      else if f_size p < 5 then begin
	      let (c1, p', c2) = f_decomp p in
	      let (c3, _, _) = f_get p' in
	      Printf.fprintf channel "\092begin{mathpar}\n\092preuve{%d}:~\n\092inferrule{" i;
	      clause channel c3 f_id f_elt;
	      Printf.fprintf channel "\n\092and";
	      clause channel c2 f_id f_elt;
	      Printf.fprintf channel "\n}{";
	      clause channel c1 f_id f_elt;
	      Printf.fprintf channel "\n}\n\092end{mathpar}\n"
      end
      else begin
	      let (c1, p', c2) = f_decomp p in
	      let (c3, p'', c4) = f_decomp p' in
	      let (c5, _, _) = f_get p'' in
	      Printf.fprintf channel "\092begin{mathpar}\n\092preuve{%d}:~\n\092inferrule{\n\092inferrule{" i;
	      clause channel c4 f_id f_elt;
	      Printf.fprintf channel "\n\092and";
	      clause channel c5 f_id f_elt;
	      Printf.fprintf channel "\n}{";
	      clause channel c3 f_id f_elt;
	      Printf.fprintf channel "\n}\092and";
	      clause channel c2 f_id f_elt;
	      Printf.fprintf channel "\n}{";
	      clause channel c1 f_id f_elt;
	      Printf.fprintf channel "\n}\n\092end{mathpar}\n"
	    end in

    iter prf 0
      
      
      

  let file a f_get f_size f_id f_elt =
    let channel = open_out "proof.tex" in
    head channel;
    proof channel a f_get f_size f_id f_elt;
    Printf.fprintf channel "\n\092end{document}";
    close_out channel;
    let _ = Sys.command "pdflatex proof.tex; rm -f *.aux *.log; evince proof.pdf &" in ()
      
end;;

module type CorElt =
sig
  type proof
  val get : proof -> int list * proof * proof
  val size : proof -> int
end;;

module Make = functor (Cor : CorElt) ->
struct
  type proof = Cor.proof
  let file a = Print.file a Cor.get Cor.size (let compt = ref 0 in fun x -> incr compt; !compt) (fun x -> x)
end;;
