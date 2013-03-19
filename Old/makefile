all :
	ocamlc -a core.ml -o core.cma
	ocamlc -a rand.ml -o rand.cma
	ocamlc -a default.ml -o default.cma
	ocamlc -a clause.ml -o clause.cma
	ocamlc -a oper.ml -o oper.cma
	ocamlc -a wlit.ml -o wlit.cma
	ocamlc -a tas.ml -o tas.cma
	ocamlc test.ml -o test
	ocamlc rand.cma default.cma -a order.ml -o order.cma
	ocamlc core.cma order.cma clause.cma oper.cma wlit.cma dpll.ml -o dpll
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe

clean :
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe
