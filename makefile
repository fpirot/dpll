all :
	ocamlc -a da.ml -o da.cma
	ocamlc da.cma -a core.ml -o core.cma
	ocamlc -a rand.ml -o rand.cma
	ocamlc -a default.ml -o default.cma
	ocamlc -a moms.ml -o moms.cma
	ocamlc -a dlis.ml -o dlis.cma
	ocamlc -a clause.ml -o clause.cma
	ocamlc -a wlit.ml -o wlit.cma
	ocamlc -a graph.ml -o graph.cma
	ocamlc -a proof.ml -o proof.cma
	ocamlc -a wlit.cma oper.ml -o oper.cma
	ocamlc rand.cma default.cma moms.cma dlis.cma -a order.ml -o order.cma
	ocamlc core.cma order.cma clause.cma oper.cma wlit.cma graph.cma proof.cma dpll.ml -o dpll
	ocamlc -a solution.ml -o solution.cma
	ocamlc solution.cma test.ml -o test
	ocamlc addition.ml -o arith
	cd tseitin/; make; cd ..
	cd enigmes/; make; cd ..
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe *.log *.aux Test/*.cmo Test/*.cmi Test/*.cma Test/*~ Test/\#*\#

generate :
	ocamlc -a da.ml -o da.cma
	ocamlc da.cma -a core.ml -o core.cma
	ocamlc -a rand.ml -o rand.cma
	ocamlc -a default.ml -o default.cma
	ocamlc -a moms.ml -o moms.cma
	ocamlc -a dlis.ml -o dlis.cma
	ocamlc -a clause.ml -o clause.cma
	ocamlc -a wlit.ml -o wlit.cma
	ocamlc -a wlit.cma oper.ml -o oper.cma
	ocamlc -a graph.ml -o graph.cma
	ocamlc -a proof.ml -o proof.cma
	ocamlc rand.cma default.cma moms.cma dlis.cma -a order.ml -o order.cma
	ocamlc Test/test.ml -o Test/test
	ocamlc core.cma order.cma clause.cma oper.cma wlit.cma graph.cma proof.cma dpll_test.ml -o Test/dpll
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe Test/*.cmo Test/*.cmi Test/*.cma Test/*~ Test/\#*\# Test\*.exe

graph :
	rm -f Graph/*.pdf Graph/*~
	if [ -d Graph/ ]; \
	  then ( for file in `ls Graph`; \
		   do dot -Tpdf Graph/$$file -o Graph/$$file.pdf; \
		 done ) ; \
	  else : ; \
	fi
	rm -f Graph/*.dot Graph/*~
	
proof :
	pdflatex proof.tex -outpout-directory Latex
	rm -rf *.log *.aux *.tex

clean :
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe *.log *.aux
