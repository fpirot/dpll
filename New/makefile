all :
	ocamlc -a basic.ml -o basic.cma
	ocamlc -a clause.ml -o clause.cma
	ocamlc -a oper.ml -o oper.cma
	ocamlc -a wlit.ml -o wlit.cma
	ocamlc basic.cma clause.cma oper.cma dpll.ml -o dpll.exe

clean :
	rm -rf *.cmo *.cmi *.cma *~ \#*\#
