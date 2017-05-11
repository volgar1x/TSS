.PHONY: build build-debug clean test debug

LIBS = -lib str

build:
	ocamlbuild $(LIBS) 'main.native'

build-debug:
	ocamlbuild -tag debug $(LIBS) 'main.byte'

clean:
	ocamlbuild -clean

test: clean build
	./main.native ../TP05/essais.txt
	./main.native ../TP07/essais.txt
	./main.native ../TP08/essais.txt
	./main.native ../TP11/essais.txt
	./main.native ../TP11-then-09/essais.txt

debug: build-debug
	ocamldebug ./main.byte ${ARGV}
