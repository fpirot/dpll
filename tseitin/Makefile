all: 
	ocamlbuild -yaccflag -v -lib unix main.native
	mv main.native tseitin

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
	rm -rf *~ \#*\#
