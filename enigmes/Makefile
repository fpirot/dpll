all: 
	ocamlc -a solution.ml -o solution.cma
	ocamlc solution.cma latin.ml -o latin
	ocamlc solution.cma color.ml -o color
	ocamlc solution.cma sudoku.ml -o sudoku
	ocamlc solution.cma magique.ml -o magique
	ocamlc addition.ml -o arith
	rm -rf *.cma *.cmi *.cmo *~ \#*\#

clean: 
	rm -rf *.cma *~ \#*\#
