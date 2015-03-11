OCB:=ocamlbuild

all: main.native

main.native: main.ml
	$(OCB) $@
