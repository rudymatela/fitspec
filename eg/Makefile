# Makefile for FitSpec

# Configuration variables
FITSPECPATH = ../src
GHCFLAGS = -O2 -i$(FITSPECPATH) -dynamic

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
