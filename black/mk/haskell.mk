# Haskell implicit rules
#
# You can optionally configure the "Configuration variables" below in your main
# makefile, e.g.:
#
#   GHCIMPORTDIRS = path/to/dir:path/to/another/dir
#   GHCFLAGS = -O2 -dynamic
#   GHC = ghc-7.6
#   include haskell.mk
#
# NOTE:
#   * Setting -O2 on GHCFLAGS will break GHCi
#                (which does not support -O2)



# Configuration variables

# GHC Parameters
GHCIMPORTDIRS ?=
GHCFLAGS ?=
GHC ?= ghc

# Makefile where to keep the dependencies
DEPMK ?= mk/depend.mk

# By default, excludes dist and Setup.hs, so that a Makefile can coexist nicely
# in a cabalized project.
LISTHS ?= find \( -path "./dist" -o -path "./Setup.hs" \) -prune \
            -o -name "*.*hs" -print
# Alternate LISTHSs:
# LISTHS = find src -name \*.hs -o -name \*.lhs  # all sources on src folder
# LISTHS = echo M1 M2 | tr ' ' '\n'              # specific files



# Implicit rules
GHCCMD = $(GHC) -i$(GHCIMPORTDIRS) $(GHCFLAGS)

%.hi %.o: %.hs
	$(GHCCMD) $< && touch $@

%: %.hs
	$(GHCCMD) $< && touch $@

.PHONY: %.ghci
%.ghci: %.hs
	$(GHCCMD) --interactive $<


# Cleaning rule (add as a clean dependency)
.PHONY: clean-hi-o
clean-hi-o:
	$(LISTHS) | sed -e "s/hs$$/o/"  | xargs rm -f
	$(LISTHS) | sed -e "s/hs$$/hi/" | xargs rm -f
	$(LISTHS) | sed -e "s/hs$$/dyn_o/"  | xargs rm -f
	$(LISTHS) | sed -e "s/hs$$/dyn_hi/" | xargs rm -f


# Update dependency file
.PHONY: depend
depend:
	$(LISTHS) | ./mk/ghcdeps $(GHCFLAGS) > $(DEPMK)

include $(DEPMK)
