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

* Make `deriveMutable` cascade.  See `bench/haskell-src.hs`.
