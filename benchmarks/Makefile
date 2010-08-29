package := containers
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
lib := ../dist/build/libHS$(package)-$(version).a

programs := bench

bench: Benchmarks.hs ../Data/Map.hs
	ghc -DTESTING -cpp -O2 --make -fforce-recomp -i.. -o bench Benchmarks.hs

.PHONY: run-bench
run-bench: bench
	./bench +RTS -K10M

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
