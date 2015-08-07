OCB=ocamlbuild -use-ocamlfind
all:
	$(OCB) main.native

pwilke:
	$(OCB) types.native
