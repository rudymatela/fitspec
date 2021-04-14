# Implicit rules for compiling Haskell code.
#
# Copyright (c) 2015-2021 Rudy Matela.
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
HADDOCK ?= haddock

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
LIST_ALL_HSS ?= find \( -path "./dist*" -o -path "./.stack-work" -o -path "./Setup.hs" \) -prune \
                     -o -name "*.*hs" -print
LIB_HSS ?= $(shell $(LIST_LIB_HSS))
ALL_HSS ?= $(shell $(LIST_ALL_HSS))

LIB_DEPS ?= base
ALL_DEPS ?= $(LIB_DEPS)

PKGNAME = $(shell cat *.cabal | grep "^name:"    | sed -e "s/name: *//")
HADDOCK_VERSION = $(shell $(HADDOCK) --version | grep version | sed -e 's/.*version //;s/,.*//')
HADDOCK_MAJOR = $(shell echo $(HADDOCK_VERSION) | sed -e 's/\..*//')
HADDOCK_MINOR = $(shell echo $(HADDOCK_VERSION) | sed -e 's/[0-9]*\.\([0-9]*\).[0-9]*/\1/')
HADDOCK_PKG_NAME = $(shell [ $(HADDOCK_MAJOR) -gt 2 ] \
                        || [ $(HADDOCK_MAJOR) -eq 2 -a $(HADDOCK_MINOR) -ge 20 ] \
                        && echo "--package-name=$(PKGNAME)")
HADDOCK_HLNK_SRC = $(shell [ $(HADDOCK_MAJOR) -gt 2 ] \
                        || [ $(HADDOCK_MAJOR) -eq 2 -a $(HADDOCK_MINOR) -ge 17 ] \
                        && echo "--hyperlinked-source")


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
clean-hs: clean-hi-o clean-haddock clean-cabal clean-stack

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
	rm -f doc/*.{html,css,js,png,gif,json} doc/src/* README.html

upload-haddock:
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"
	@echo "(on Arch Linux, use: cabal haddock --for-hackage --haddock-options=--optghc=-dynamic)"

doc/index.html: $(LIB_HSS)
	./mk/haddock-i $(LIB_DEPS) | xargs \
	$(HADDOCK) --html -odoc $(LIB_HSS) \
	  --title=$(PKGNAME) \
	  $(HADDOCK_PKG_NAME) \
	  $(HADDOCK_HLNK_SRC) \
	  $(HADDOCKFLAGS)

clean-cabal:
	rm -rf dist/ dist-newstyle/ cabal.project.local cabal.project.local~

clean-stack:
	rm -rf .stack-work/ stack.yaml.lock

# lists all Haskell source files
list-all-hss:
	@find $(ALL_HSS)

# lists library Haskell source files
list-lib-hss:
	@find $(LIB_HSS)

bootstrap-haskell-mk:
	@[ -d "$(DEST)" ] || (echo -e "error: no destination found\nusage: \`make bootstrap-haskell-mk DEST=path/to/prj'"; exit 1)
	mkdir -p mk
	cp mk/{haskell.mk,ghcdeps,haddock-i} $(DEST)/mk
	touch $(DEST)/mk/depend.mk

show-pkgname:
	@echo $(PKGNAME)

include $(DEPMK)
