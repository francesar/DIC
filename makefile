# build:
# 	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 toplevel.native

# test: build
# 	./run-all-tests.sh

# test-menhir:build
# 	menhir --interpret --interpret-show-cst parser.mly

# test-helloworld: build
# 	./testhelloworld.sh


.PHONY : all
all : toplevel.native len.o io.o


.PHONY: toplevel.native
toplevel.native:
	rm -f len.o io.o
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
	toplevel.native

.PHONY: clean

clean:
	rm toplevel.native && rm -rf _build && len.o
