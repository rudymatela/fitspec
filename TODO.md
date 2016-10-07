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


v0.4.0
------

* release!


v0.4.1
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
