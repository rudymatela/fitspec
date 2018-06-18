# Implicit rules for compiling Haskell code.
#
# Copyright (c) 2015-2018 Rudy Matela.
# Distributed under the 3-Clause BSD licence.
#
# You can optionally configure the "Configuration variables" below in your main
# makefile, e.g.:
#
#   GHCIMPORTDIRS = path/to/dir:path/to/another/dir
#   GHCFLAGS = -O2 -dynamic
#   GHC = ghc-7.6
#   include haskell.mk



# Configuration variables

# GHC Parameters
GHCIMPORTDIRS ?=
GHCFLAGS ?=
GHC ?= ghc
GHCCMD = $(GHC) -i$(GHCIMPORTDIRS) $(GHCFLAGS)

# Hugs Parameters
HUGSIMPORTDIRS ?= "/usr/lib/hugs/packages/*"
HUGSFLAGS ?=
CPPHS_HUGS ?= cpphs-hugs --noline -D__HUGS__
HUGS ?= hugs
RUNHUGS ?= runhugs
HUGSCMD    = $(HUGS)    -F"$(CPPHS_HUGS)" -P$(HUGSIMPORTDIRS) $(HUGSFLAGS)
RUNHUGSCMD = $(RUNHUGS) -F"$(CPPHS_HUGS)" -P$(HUGSIMPORTDIRS) $(HUGSFLAGS)


# Makefile where to keep the dependencies
DEPMK ?= mk/depend.mk

# LIB_HSS: all library Haskell files
# ALL_HSS: all Haskell files
# You can override ALL/LIB_HSS in your main Makefile
LIST_LIB_HSS ?= find src -name "*.hs"
LIST_ALL_HSS ?= find \( -path "./dist" -o -path "./.stack-work" \) -prune \
                     -o -name "*.*hs" -print
LIB_HSS ?= $(shell $(LIST_LIB_HSS))
ALL_HSS ?= $(shell $(LIST_ALL_HSS))

PKGNAME = $(shell cat *.cabal | grep "^name:"    | sed -e "s/name: *//")


# Implicit rules
%.hi %.o: %.hs
	$(GHCCMD) $< && touch $@

%: %.hs
	$(GHCCMD) $< && touch $@

.PHONY: %.ghci
%.ghci: %.hs
	$(GHCCMD) -O0 --interactive $<

.PHONY: %.hugs
%.hugs: %.hs
	$(HUGSCMD) $<

.PHONY: %.runhugs
%.runhugs: %.hs
	$(RUNHUGSCMD) $<


# Cleaning rule (add as a clean dependency)
.PHONY: clean-hi-o
clean-hi-o:
	find $(ALL_HSS) | sed -e 's/hs$$/o/'      | xargs rm -f
	find $(ALL_HSS) | sed -e 's/hs$$/hi/'     | xargs rm -f
	find $(ALL_HSS) | sed -e 's/hs$$/dyn_o/'  | xargs rm -f
	find $(ALL_HSS) | sed -e 's/hs$$/dyn_hi/' | xargs rm -f


# Update dependency file
.PHONY: depend
depend:
	find $(ALL_HSS) | ./mk/ghcdeps -i$(GHCIMPORTDIRS) $(GHCFLAGS) > $(DEPMK)

# haddock rules
haddock: doc/index.html

clean-haddock:
	rm -f doc/*.{html,css,js,png,gif,json} README.html

upload-haddock:
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"
	@echo "(on Arch Linux, use: cabal haddock --for-hackage --haddock-options=--optghc=-dynamic)"

doc/index.html: $(LIB_HSS)
	./mk/haddock-i base template-haskell | xargs \
	haddock --html -odoc $(LIB_HSS) $(HADDOCKFLAGS) --title=$(PKGNAME)

# lists all Haskell source files
list-all-hss:
	@find $(ALL_HSS)

# lists library Haskell source files
list-lib-hss:
	@find $(LIB_HSS)

show-pkgname:
	@echo $(PKGNAME)

include $(DEPMK)
