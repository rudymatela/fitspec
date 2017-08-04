# Makefile for FitSpec
#
# Copyright:   (c) 2015-2017 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
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
LISTHS=find eg src tests bench -name \*.hs
HSS=$(shell $(LISTHS))
LISTLIBS=find src -name \*.hs
OBJS = $(shell $(LISTLIBS) | sed -e 's/hs$$/o/')
MOSTBENCHS = \
  bench/avltrees         \
  bench/bools            \
  bench/digraphs         \
  bench/heaps            \
  bench/id               \
  bench/list             \
  bench/mergeheaps       \
  bench/pretty           \
  bench/sets             \
  bench/setsofsets       \
  bench/sieve            \
  bench/sorting          \
  bench/spring
BENCHS = $(MOSTBENCHS) \
  bench/haskell-src      \
  bench/haskell-src-exts
EGS = \
  eg/sorting \
  eg/negation
TESTS = \
  tests/test-derive      \
  tests/test-mutate      \
  tests/test-showmutable \
  tests/test-utils

all: $(OBJS)

benchs: $(BENCHS) all

egs: $(EGS) all

ghci: Test/FitSpec.ghci

test: all benchs egs $(TESTS)
	./tests/test-mutate
	./tests/test-showmutable
	./tests/test-derive
	./tests/test-utils

test-without-hs-src: all $(MOSTBENCHS) egs $(TESTS)
	./tests/test-mutate
	./tests/test-showmutable
	./tests/test-derive
	./tests/test-utils

legacy-test:
	make clean && make -C $(LEANCHECKPATH) clean && make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make -C $(LEANCHECKPATH) clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean

prepare-legacy-test:
	cabal-ghc-7.10 --ignore-sandbox install --ghc-option=-dynamic cpphs haskell-src-exts
	cabal-ghc-7.8  --ignore-sandbox install --ghc-option=-dynamic cpphs haskell-src-exts
	cabal-ghc-7.10 --ignore-sandbox install haskell-src
	cabal-ghc-7.8  --ignore-sandbox install haskell-src

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
	cabal-ghc-7.10 configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-7.10 build && cabal-ghc-7.10 test
	cabal clean && cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.8  configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-7.8  build && cabal-ghc-7.8  test
	cabal clean

prepare-legacy-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal sandbox add-source $(LEANCHECKPATH)
	cabal          install --only-dependencies --enable-tests --enable-benchmarks
	cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.10 install --only-dependencies --enable-tests --enable-benchmarks
	cd $(LEANCHECKPATH) && cabal clean && cd -
	cabal-ghc-7.8  install --only-dependencies --enable-tests --enable-benchmarks

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS) $(EGS) {eg,bench}/*.{hi,o,dyn_hi,dyn_o}

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

upload-haddock:
	./mk/upload-haddock-to-hackage

doc/index.html: $(shell $(LISTLIBS))
	./mk/haddock-i base template-haskell | xargs \
	haddock --html --no-print-missing-docs --title=fitspec \
	  --optghc=-i$(GHCIMPORTDIRS) \
	  -odoc $(shell $(LISTLIBS))

bench/avltrees: bench/AVLTree.o

bench/heaps: bench/Heap.o

bench/digraphs: bench/Digraph.o

include mk/haskell.mk
