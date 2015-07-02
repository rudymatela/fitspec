Pattern Mutants
===============

To run/load this library, [sized-functors] and enumerable libraries are needed.

To load from a sandboxed ghci:

	cabal sandbox init
	cabal sandbox add-source /path/to/sized-functors/
	cabal update
	cabal install sized-functors
	cabal install enumerable
	ghci -package-db .cabal-sandbox/*-packages.conf.d  Mutants.hs


[sized-functors]: https://github.com/JonasDuregard/sized-functors
