all:

%-properties: %-properties.hs force
	ghc -O2 -DTESTING $< -i.. -o $@ -outputdir tmp

%-strict-properties: %-properties.hs force
	ghc -O2 -DTESTING -DSTRICT $< -o $@ -i.. -outputdir tmp

.PHONY: force clean
force:

clean:
	rm -rf tmp $(patsubst %.hs, %, $(wildcard *-properties.hs)) $(patsubst %-properties.hs, %-strict-properties, $(wildcard *-properties.hs))
