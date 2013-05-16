all: 
	ocamlc -a solution.ml -o solution.cma
	ocamlc solution.cma latin.ml -o latin
	ocamlc solution.cma color.ml -o color
	ocamlc solution.cma latin2.ml -o latin2
	ocamlc solution.cma magic.ml -o magic
	ocamlc solution.cma sudoku.ml -o sudoku
	ocamlc solution.cma magique.ml -o magique
	rm -rf *.cma *.cmi *.cmo *~ \#*\#

clean: 
	rm -rf *.cma *~ \#*\#
