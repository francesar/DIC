build:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 toplevel.native

test: build
	./testscript

.PHONY: clean

clean:
	rm toplevel.native && rm -rf _build