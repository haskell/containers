package := containers
version := $(shell awk '/^version:/{print $$2}' ../$(package).cabal)
lib := ../dist/build/libHS$(package)-$(version).a

programs := bench-Map bench-Set bench-IntMap bench-IntSet bench-Sequence
all: $(programs)
run: $(patsubst %, %.csv, $(programs))

bench-%: %.hs ../Data/%.hs
	ghc -DTESTING -cpp -O2 -fregs-graph -rtsopts --make -fforce-recomp -i.. -o $@ $<

bench-%.csv: bench-%
	./bench-$* -u bench-$*.csv +RTS -K10M

.PHONY: clean
clean:
	-find . \( -name '*.o' -o -name '*.hi' \) -exec rm {} \;
	-rm -f $(programs)
