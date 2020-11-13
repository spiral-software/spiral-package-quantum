
.PHONY: lab*

default: lab
lab*: bin
	$(MAKE) -C $@

bin:
	mkdir bin

clean:
	rm -Rf bin
	for l in lab*; do $(MAKE) -C $$l clean; done
