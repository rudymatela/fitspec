# Makefile for FitSpec
#
# Copyright:   (c) 2015-2020 Rudy Matela
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
GHCFLAGS = -v0 -O2 \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic -package leancheck -package cmdargs)
CABALOPTS=
BENCHS = \
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
EXTRA_BENCHS = $(BENCHS) \
  bench/haskell-src      \
  bench/haskell-src-exts
EGS = \
  eg/sorting \
  eg/negation
EXTRA_EGS = $(EGS) \
  eg/alga
TESTS = \
  test/derive      \
  test/mutate      \
  test/showmutable \
  test/utils
LIST_ALL_HSS = find \( -path "./dist*" -o -path "./.stack-work" -o -path "./Setup.hs" -o -name "haskell-src*.hs" \) -prune \
                    -o -name "*.*hs" -print
HADDOCKFLAGS = $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic)
LIB_DEPS = base $(INSTALL_DEPS)
INSTALL_DEPS = leancheck cmdargs template-haskell pretty

all: mk/toplibs

benchs: $(BENCHS) all

egs: $(EGS) all

ghci: Test/FitSpec.ghci

test: all benchs egs $(TESTS)
	./test/mutate
	./test/showmutable
	./test/derive
	./test/utils

test-with-extra-deps: all $(EXTRA_BENCHS) $(EXTRA_EGS) $(TESTS)
	./test/mutate
	./test/showmutable
	./test/derive
	./test/utils

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test mutate

test-via-stack:
	stack test fitspec:test:mutate --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

test-sdist:
	./test/sdist

legacy-test:
	make clean && make test -j8 GHC=ghc-8.2  GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-8.0  GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean && make test                    -j8

legacy-test-via-cabal:
	cabal-ghc-8.2  configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-8.2  build && cabal-ghc-8.2  test
	cabal-ghc-8.0  configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-8.0  build && cabal-ghc-8.0  test
	cabal-ghc-7.10 configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-7.10 build && cabal-ghc-7.10 test
	cabal-ghc-7.8  configure --enable-tests --enable-benchmarks --ghc-option=-dynamic && cabal-ghc-7.8  build && cabal-ghc-7.8  test
	cabal clean

prepare-legacy-test-via-cabal:
	rm -rf .cabal-sandbox cabal.sandbox.config
	cabal sandbox init
	cabal          install --only-dependencies --enable-tests --enable-benchmarks --enable-documentation
	cabal-ghc-8.2  install --only-dependencies --enable-tests --enable-benchmarks
	cabal-ghc-8.0  install --only-dependencies --enable-tests --enable-benchmarks
	cabal-ghc-7.10 install --only-dependencies --enable-tests --enable-benchmarks
	cabal-ghc-7.8  install --only-dependencies --enable-tests --enable-benchmarks

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS) $(EGS) {eg,bench}/*.{hi,o,dyn_hi,dyn_o} TAGS tags mk/toplibs

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

mk/toplibs: src/Test/FitSpec.o bench/Set.o
	touch mk/toplibs

bench/avltrees: bench/AVLTree.o

bench/heaps: bench/Heap.o

bench/digraphs: bench/Digraph.o

include mk/haskell.mk
# NOTE:
#
# To run make depend, you may need to pass -package now to expose a package:
#
# make depend GHCFLAGS="-package haskell-src-exts"
