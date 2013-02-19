all :
	ocamlc -a dpll.ml -o dpll.cma
	ocamlc dpll.ml -o dpll.exe
	ocamlc -a wlit.ml -o wlit.cma

clean :
	rm -rf *.cmo *.cmi *.cma *~
