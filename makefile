build:
	ocamlbuild toplevel.native

test: build
	./testscript

menhir:
	menhir --interpret --interpret-show-cst parser.mly

.PHONY: clean

clean:
	rm toplevel.native && rm -rf _build