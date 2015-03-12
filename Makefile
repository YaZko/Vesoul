OCB:=ocamlbuild
SRC:=$(wildcard *.ml Makefile)

all: main.native

zip:
	zip vesoul $(SRC)

main.native: $(SRC)
	$(OCB) $@

test:
	cat ex.in | ./main.native

run:
	cat dc.in | ./main.native


clean:
	$(OCB) -clean
