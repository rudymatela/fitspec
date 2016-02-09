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
LLCHECKPATH = ../llcheck

# Misc variables
GHCIMPORTDIRS = $(LLCHECKPATH):bench
GHCFLAGS = -dynamic
LISTHS=find -name \*.hs \
            -a \! \( -path "./dist/*" \
                  -o -path "./patterns/*" \
                  -o -path "./Setup.hs" \)
OBJS = $(shell $(LISTHS) | sed -e 's/hs$$/o/')
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
	make clean && make -C $(LLCHECKPATH) clean && make test GHC=ghc-7.6 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean && make -C $(LLCHECKPATH) clean && make test GHC=ghc-7.4 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean

test-via-cabal:
	cabal test

prepare-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal sandbox add-source $(LLCHECKPATH)
	cabal install --only-dependencies
	cabal configure --enable-tests --enable-benchmarks

legacy-test-via-cabal:
	cabal clean && cd $(LLCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.8 configure --enable-tests --enable-benchmarks && cabal-ghc-7.8 build && cabal-ghc-7.8 test
	cabal clean && cd $(LLCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.6 configure --enable-shared --enable-tests --enable-benchmarks && cabal-ghc-7.6 build && cabal-ghc-7.6 test
	cabal clean && cd $(LLCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.4 configure --enable-shared --enable-tests --enable-benchmarks && cabal-ghc-7.4 build && cabal-ghc-7.4 test
	cabal clean

prepare-legacy-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal sandbox add-source $(LLCHECKPATH)
	cabal         install --only-dependencies
	cd $(LLCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.8 install --only-dependencies
	cd $(LLCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.6 install --only-dependencies
	cd $(LLCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.4 install --only-dependencies

clean: clean-hi-o
	rm -f $(TESTS) $(BENCHS)

# Debug: just list all source files compiled normally
list:
	$(LISTHS)

hlint:
	hlint \
	  --ignore "Use first" \
	  --ignore "Use ***" \
	  .

bench/avltrees: bench/AVLTree.o

bench/heaps: bench/Heap.o

include mk/haskell.mk
