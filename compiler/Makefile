.PHONY: all
all:
	ghc -fno-warn-overlapping-patterns --make -O2 Page.hs -o page
	ghc -fno-warn-overlapping-patterns --make -O2 Main.hs -o wildfire

.PHONY: clean
clean:
	rm -f *.hi *.o *.v *.page page wildfire
