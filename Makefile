OCB:=ocamlbuild -use-ocamlfind

all: main.native

main.native: main.ml
	$(OCB) $@
