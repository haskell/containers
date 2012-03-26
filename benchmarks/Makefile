all:

bench-%: %.hs *.hs ../Data/*.hs ../Data/*/*.hs
	ghc -O2 -DTESTING $< -i.. -o $@ -outputdir tmp -rtsopts

bench-%.csv: bench-%
	./bench-$* -v -u bench-$*.csv

.PHONY: clean veryclean
clean:
	rm -rf tmp $(patsubst %.hs, bench-%, $(wildcard *.hs))

veryclean: clean
	rm -rf $(patsubst %.hs, bench-%.csv, $(wildcard *.hs))
