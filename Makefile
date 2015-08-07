OCB=ocamlbuild -use-ocamlfind -cflag -g
all:
	$(OCB) main.native

pwilke:
	$(OCB) types.native

clean:
	rm -rf _build
