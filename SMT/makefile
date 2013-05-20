all:
	ocamlbuild -yaccflag -v -lib unix smt.native; ln -fs smt.native smt

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
