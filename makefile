build:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 toplevel.native

test: build
	./testscript

test-helloworld: build
	./testhelloworld.sh

.PHONY: clean

clean:
	rm toplevel.native && rm -rf _build && rm test.ll test.exe test.s