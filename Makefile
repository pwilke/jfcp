OCB=ocamlbuild -use-ocamlfind -cflag -g
all:
	$(OCB) main.native

pwilke:
	$(OCB) types.native

clean:
	rm -rf _build

archive:
	tar cf jfcp.tar.gz src/*.ml src/*.mli Makefile problems/problem_*.json README
