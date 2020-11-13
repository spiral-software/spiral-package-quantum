all: unparser

unparser: ../bin/unparser
../bin/unparser: always
	dune build
	mkdir -p ../bin
	install _build/default/bin/unparser.exe $@

always:

clean:
	dune clean


.PHONY: unparser clean native
