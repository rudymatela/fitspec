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
LEANCHECKPATH = ../leancheck

# Misc variables
GHCIMPORTDIRS = src:$(LEANCHECKPATH)/src:bench
GHCFLAGS = -dynamic -O2
LISTHS=find eg src tests -name \*.hs
HSS=$(shell $(LISTHS))
LISTLIBS=find src -name \*.hs
OBJS = $(shell $(LISTLIBS) | sed -e 's/hs$$/o/')
BENCHS = \
  bench/avltrees   \
  bench/bools      \
  bench/digraphs   \
  bench/heaps      \
  bench/id         \
  bench/list       \
  bench/mergeheaps \
  bench/pretty     \
  bench/sets       \
  bench/setsofsets \
  bench/sieve      \
  bench/sorting    \
  bench/spring
TESTS = \
  tests/test-derive      \
  tests/test-mutate      \
  tests/test-showmutable \
  tests/test-utils

all: $(OBJS)

benchs: $(BENCHS) all

ghci: FitSpec.ghci

test: all benchs $(TESTS)
	./tests/test-mutate
	./tests/test-showmutable
	./tests/test-derive
	./tests/test-utils

legacy-test:
	make clean && make -C $(LEANCHECKPATH) clean && make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make -C $(LEANCHECKPATH) clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean && make -C $(LEANCHECKPATH) clean && make test GHC=ghc-7.6 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean && make -C $(LEANCHECKPATH) clean && make test GHC=ghc-7.4 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean

test-via-cabal:
	cabal test

prepare-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal sandbox add-source $(LEANCHECKPATH)
	cabal install --only-dependencies
	cabal configure --enable-tests --enable-benchmarks

legacy-test-via-cabal:
	cabal clean && cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.10 configure --enable-tests --enable-benchmarks && cabal-ghc-7.10 build && cabal-ghc-7.10 test
	cabal clean && cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.8 configure --enable-tests --enable-benchmarks && cabal-ghc-7.8 build && cabal-ghc-7.8 test
	cabal clean && cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.6 configure --enable-shared --enable-tests --enable-benchmarks && cabal-ghc-7.6 build && cabal-ghc-7.6 test
	cabal clean && cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.4 configure --enable-shared --enable-tests --enable-benchmarks && cabal-ghc-7.4 build && cabal-ghc-7.4 test
	cabal clean

prepare-legacy-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal sandbox add-source $(LEANCHECKPATH)
	cabal         install --only-dependencies
	cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.8 install --only-dependencies
	cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.6 install --only-dependencies
	cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.4 install --only-dependencies

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS)

# Debug: just list all source files compiled normally
list-hs:
	$(LISTHS)

list-libs:
	$(LISTLIBS)

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Use first" \
	  --ignore "Use second" \
	  --ignore "Use ***" \
	  FitSpec.hs FitSpec bench tests

haddock: doc/index.html

clean-haddock:
	rm -f doc/*.{html,css,js,png,gif}

doc/index.html: $(shell $(LISTLIBS))
	./mk/haddock-i base template-haskell | xargs \
	haddock --html --no-print-missing-docs --title=fitspec \
	  --optghc=-i$(GHCIMPORTDIRS) \
	  -odoc $(shell $(LISTLIBS))

bench/avltrees: bench/AVLTree.o

bench/heaps: bench/Heap.o

bench/digraphs: bench/Digraph.o

include mk/haskell.mk
