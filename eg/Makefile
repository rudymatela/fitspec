# Makefile for FitSpec

# Configuration variables
FITSPECPATH   = ..
LEANCHECKPATH = ../../leancheck
GHCFLAGS = -O2 -i$(LEANCHECKPATH):$(FITSPECPATH)

# Misc variables

all: sorting negation

clean:
	rm -f *.hi *.o sorting negation

# Implicit rules:

%.o: %.hs
	ghc $(GHCFLAGS) $<

%: %.hs
	ghc $(GHCFLAGS) $<

.PHONY: %.ghci
%.ghci: %.hs
	ghci $(GHCFLAGS) -O0 $<
