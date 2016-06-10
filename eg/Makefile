# Makefile for FitSpec

# Configuration variables
FITSPECPATH   = ..
LEANCHECKPATH = ../../leancheck
GHCFLAGS = -O2 -i$(LEANCHECKPATH):$(FITSPECPATH)

# Misc variables

all: sorting

clean:
	rm -f *.hi *.o sorting

# Implicit rules:

%.o: %.hs
	ghc $(GHCFLAGS) $<

%: %.hs
	ghc $(GHCFLAGS) $<

.PHONY: %.ghci
%.ghci: %.hs
	ghci $(GHCFLAGS) -O0 $<
