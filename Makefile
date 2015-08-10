OCB=ocamlbuild -use-ocamlfind -cflag -g
all:
	$(OCB) main.native
	cp -fL main.native play_icfp2015

pwilke:
	$(OCB) types.native

clean:
	rm -rf _build

archive:
	tar czf jfcp.tar.gz src/*.ml src/*.mli Makefile problems/problem_*.json README default.nix _tags
