OCB:=ocamlbuild
SRC:=$(wildcard *.ml Makefile)

all: main.native

zip:
	zip vesoul $(SRC)

main.native: $(SRC)
	$(OCB) $@

clean:
	$(OCB) -clean
