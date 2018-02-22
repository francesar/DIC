build:
	ocamlbuild toplevel.native

test: build
	./testscript

.PHONY: clean

clean:
	rm toplevel.native && rm -rf _build