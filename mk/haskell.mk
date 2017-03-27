# Implicit rules for compiling Haskell code.
#
# Copyright (c) 2015-2017 Rudy Matela.
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

# Makefile where to keep the dependencies
DEPMK ?= mk/depend.mk

# By default, excludes dist and Setup.hs, so that a Makefile can coexist nicely
# in a cabalized project.
HSS ?= $(shell find \( -path "./dist" -o -path "./Setup.hs" \) -prune \
                 -o -name "*.*hs" -print)
# You can override HSS in your main Makefile
# It should include all Haskell sources to be compiled by different targets
# (even those that are not a dependency to all)
# It will be used to generate dependencies and for cleaning objects



# Implicit rules
GHCCMD = $(GHC) -i$(GHCIMPORTDIRS) $(GHCFLAGS)

%.hi %.o: %.hs
	$(GHCCMD) $< && touch $@

%: %.hs
	$(GHCCMD) $< && touch $@

.PHONY: %.ghci
%.ghci: %.hs
	$(GHCCMD) -O0 --interactive $<


# Cleaning rule (add as a clean dependency)
.PHONY: clean-hi-o
clean-hi-o:
	find $(HSS) | sed -e 's/hs$$/o/'      | xargs rm -f
	find $(HSS) | sed -e 's/hs$$/hi/'     | xargs rm -f
	find $(HSS) | sed -e 's/hs$$/dyn_o/'  | xargs rm -f
	find $(HSS) | sed -e 's/hs$$/dyn_hi/' | xargs rm -f


# Update dependency file
.PHONY: depend
depend:
	find $(HSS) | ./mk/ghcdeps -i$(GHCIMPORTDIRS) $(GHCFLAGS) > $(DEPMK)

include $(DEPMK)
