all:

%-properties: %-properties.hs ../Data/*.hs ../Data/*/*.hs
	ghc -O2 -DTESTING $< -i.. -o $@ -outputdir tmp

%-strict-properties: %-properties.hs
	ghc -O2 -DTESTING -DSTRICT $< -o $@ -i.. -outputdir tmp

.PHONY: clean
clean:
	rm -rf tmp $(patsubst %.hs, %, $(wildcard *-properties.hs)) $(patsubst %-properties.hs, %-strict-properties, $(wildcard *-properties.hs))
