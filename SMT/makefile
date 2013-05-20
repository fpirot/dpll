all:
	ocamlbuild -yaccflag -v -lib unix main.native; ln -fs main.native smt

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
