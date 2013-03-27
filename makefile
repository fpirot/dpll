all :
	ocamlc -a core.ml -o core.cma
	ocamlc -a rand.ml -o rand.cma
	ocamlc -a default.ml -o default.cma
	ocamlc -a moms.ml -o moms.cma
	ocamlc -a dlis.ml -o dlis.cma
	ocamlc -a clause.ml -o clause.cma
	ocamlc -a oper.ml -o oper.cma
	ocamlc -a wlit.ml -o wlit.cma
	ocamlc rand.cma default.cma moms.cma dlis.cma -a order.ml -o order.cma
	ocamlc core.cma order.cma clause.cma oper.cma wlit.cma dpll.ml -o dpll
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe

test :
	ocamlc -a core.ml -o core.cma
	ocamlc -a rand.ml -o rand.cma
	ocamlc -a default.ml -o default.cma
	ocamlc -a moms.ml -o moms.cma
	ocamlc -a dlis.ml -o dlis.cma
	ocamlc -a clause.ml -o clause.cma
	ocamlc -a oper.ml -o oper.cma
	ocamlc -a wlit.ml -o wlit.cma
	ocamlc rand.cma default.cma moms.cma dlis.cma -a order.ml -o order.cma
	ocamlc Test/test.ml -o Test/test
	ocamlc core.cma order.cma clause.cma oper.cma wlit.cma dpll_test.ml -o Test/dpll
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe Test/*.cmo Test/*.cmi Test/*.cma Test/*~ Test/\#*\# Test\*.exe

clean :
	rm -rf *.cmo *.cmi *.cma *~ \#*\# *.exe
