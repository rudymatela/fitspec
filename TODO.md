TO DO list for FitSpec
======================

misc
----

* (?) announce on mailing list (wait for Haddock to be ready)

* parameterize number of tests in test programs and add slow-test target

* add diff test for IO functions (diff w/ model output and exit status)


documentation
-------------

* add second simple/minimal example in eg
  (unbalanced binary search tree?  stack?);

* write detailed install instructions on INSTALL.md
  (cabal install, cabal from sandbox, source include)


v0.3.2
------

* Fix `thread killed` when first run takes longer than timeout:

	$ ./bench/haskell-src -t5 -m500 -n1000
	haskell-src: thread killed

  As it takes more than 5 seconds to run this example (at least on my machine),
  the thread will be killed before concluding.  I *think* this is the source of
  the bug, but could be something else.

  EDIT (after re-reading the code and doing some tests): maybe the Thread
  Killed exception somehow "infects" `x` as it may share some structure with
  elements of `xs`.

  To solve this, maybe re-implement by probing (every 100ms) whether the IORef
  was ever written to --- for that I will need a second boolean IORef.  This has
  the advantage of, by adding a *third* boolean IORef we can also exit earlier
  when test cases and mutants are exhausted.

* Fix legacy-test:

	$ ghc-7.10 -isrc:../leancheck/src:bench -Werror -dynamic bench/haskell-src-exts.hs && touch bench/haskell-src-exts
	[22 of 22] Compiling Main             ( bench/haskell-src-exts.hs, bench/haskell-src-exts.o )

	bench/haskell-src-exts.hs:13:24:
		Not in scope: type constructor or class ‘SrcSpanInfo’
		In the Template Haskell quotation ''SrcSpanInfo
	make[1]: *** [mk/haskell.mk:41: bench/haskell-src-exts] Error 1
	make[1]: Leaving directory '/path/to/fitspec'
	make: *** [Makefile:62: legacy-test] Error 2


* Release: a few things have been added (check `git log`).
