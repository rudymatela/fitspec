# Haskell implicit rules
#
# Configure variables below *before* importing this into your main makefile



# Configuration variables

# GHC Parameters
GHCIMPORTDIRS ?=
GHCFLAGS ?= -i$(GHCIMPORTDIRS)

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
GHC = ghc $(GHCFLAGS)

%.hi %.o: %.hs
	$(GHC) $< && touch $@

%: %.hs
	$(GHC) $< && touch $@

.PHONY: %.ghci
%.ghci: %.hs
	$(GHC) $<


# Cleaning rule (add as a clean dependency)
.PHONY: clean-hi-o
clean-hi-o:
	$(LISTHS) | sed -e "s/hs$$/o/"  | xargs rm -f
	$(LISTHS) | sed -e "s/hs$$/hi/" | xargs rm -f


# Update dependency file
.PHONY: depend
depend:
	$(LISTHS) | ./mk/ghcdeps $(GHCFLAGS) > $(DEPMK)

include $(DEPMK)
