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

# Misc variables
GHCIMPORTDIRS = src:bench
GHCFLAGS = -dynamic -O2
HADDOCKFLAGS = --no-print-missing-docs
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
MOSTEGS = \
  eg/sorting \
  eg/negation
EGS = $(MOSTEGS) \
  eg/alga
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

test-without-extra-deps: all $(MOSTBENCHS) $(MOSTEGS) $(TESTS)
	./tests/test-mutate
	./tests/test-showmutable
	./tests/test-derive
	./tests/test-utils

legacy-test:
	make clean && make test-without-extra-deps -j8 GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make test-without-extra-deps -j8 GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean && make test                    -j8

test-via-cabal:
	cabal test

test-sdist:
	tests/test-sdist

prepare-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal install --only-dependencies --enable-documentation
	cabal configure --enable-tests --enable-benchmarks

legacy-test-via-cabal:
	cabal-ghc-7.10 configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-7.10 build && cabal-ghc-7.10 test
	cabal-ghc-7.8  configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-7.8  build && cabal-ghc-7.8  test
	cabal clean

prepare-legacy-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal          install --only-dependencies --enable-tests --enable-benchmarks --enable-documentation
	cabal-ghc-7.10 install --only-dependencies --enable-tests --enable-benchmarks
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
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"

doc/index.html: $(shell $(LISTLIBS))
	./mk/haddock-i base template-haskell | xargs \
	haddock --html $(HADDOCKFLAGS) --title=fitspec \
	  --optghc=-i$(GHCIMPORTDIRS) \
	  -odoc $(shell $(LISTLIBS))

mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

bench/avltrees: bench/AVLTree.o

bench/heaps: bench/Heap.o

bench/digraphs: bench/Digraph.o

include mk/haskell.mk
