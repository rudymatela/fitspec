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
GHCIMPORTDIRS = $(LLCHECKPATH):bench
GHCFLAGS = -dynamic
FINDHS=find -name \*.hs \
            -a \! \( -path "./dist/*" \
                  -o -path "./bench/*" \
                  -o -path "./tests/*" \
                  -o -path "./Setup.hs" \)
OBJS = $(shell $(FINDHS) | sed -e 's/hs$$/o/')
BENCHS = bench/avltrees \
         bench/bools \
		 bench/heaps \
		 bench/id \
		 bench/list \
		 bench/pretty \
		 bench/sieve \
		 bench/sorting \
		 bench/spring
TESTS = tests/test-mutate \
        tests/test-derive

all: $(OBJS)

benchs: $(BENCHS) all

test: all benchs $(TESTS)
	./tests/test-mutate
	./tests/test-derive

legacy-test:
	make clean && make -C $(LLCHECKPATH) clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean && make -C $(LLCHECKPATH) clean && make test GHC=ghc-7.6 GHCFLAGS=-Werror
	make clean && make -C $(LLCHECKPATH) clean && make test GHC=ghc-7.4 GHCFLAGS=-Werror
	make clean

legacy-test-via-cabal:
	cabal clean && cabal-ghc-7.8 configure --enable-tests --enable-benchmarks && cabal-ghc-7.8 build && cabal-ghc-7.8 test
	cabal clean && cabal-ghc-7.6 configure --enable-tests --enable-benchmarks && cabal-ghc-7.6 build && cabal-ghc-7.6 test
	cabal clean && cabal-ghc-7.4 configure --enable-tests --enable-benchmarks && cabal-ghc-7.4 build && cabal-ghc-7.4 test
	cabal clean


clean: clean-hi-o
	rm -f $(TESTS) $(BENCHS)

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

include mk/haskell.mk
