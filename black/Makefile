# Makefile for FitSpec
#
# This is faster than Cabal:
#
# Time      Scratch  Already compiled
# Makefile    14s      0s
# Cabal       90s      1s
#
# Cabal tries to "sandbox" the build of every benchmark program
# rebuilding everything for each of those.

# Configuration variables
LLCHECKPATH = ../../llcheck

# Misc variables
GHCFLAGS = -i$(LLCHECKPATH):bench
FINDHS=find -name \*.hs \
            -a \! \( -path "./dist/*" \
                  -o -path "./bench/*" \
                  -o -path "./tests/*" \
                  -o -path "./Setup.hs" \)
OBJS = $(shell $(FINDHS) | sed -e 's/hs$$/o/')
INFS = $(shell $(FINDHS) | sed -e 's/hs$$/hi/')
BENCHS = bench/avltrees \
         bench/bools \
		 bench/heaps \
		 bench/id \
		 bench/list \
		 bench/pretty \
		 bench/sieve \
		 bench/sorting \
		 bench/spring
TESTS = tests/test-mutate

all: $(OBJS) $(INFS)

benchs: $(BENCHS) all

test: all $(TESTS) $(BENCHS)
	./tests/test-mutate

clean:
	rm -f $(OBJS) $(INFS) $(TESTS) $(BENCHS)
	rm -f {tests,bench}/*.{hi,o}

# Debug: just list all source files compiled normally
list:
	$(FINDHS)

hlint:
	hlint \
	  --ignore "Use first" \
	  --ignore "Use ***" \
	  .

bench/avltrees: bench/AVLTree.o

bench/heaps: bench/Heap.o

# Implicit rules:

%.o: %.hs
	ghc $(GHCFLAGS) $<

%: %.hs
	ghc $(GHCFLAGS) $<

.PHONY: %.ghci
%.ghci: %.hs
	ghci $(GHCFLAGS) $<
