FitSpec
=======

Find minimal and complete sub-sets of properties
describing a Haskell program
using black-box mutation testing.

**This a work in progress: currently in very early stage**


Compiling and Running
---------------------

You should install the needed packages [cmdargs] and [pretty], see the
following section.

Then, you can choose how you can compile and run the tools:

* via Cabal
* via GHC directly


### Needed Packages

To run some of the benchmarks you'll need to install [cmdargs] and [pretty].
You can install them using cabal:

	cabal install cmdargs
	cabal install pretty


It is also fine to install them on a sandbox (cabal >= 1.18 is needed)

You will also need [llcheck] to run this.
Later sections describe the ways you can install it.
You should clone the [llcheck] repository
preferentially on the same folder as the [fitspec] repository:

	git clone ssh://git@github.com/rudymatela/llcheck


### Via Cabal

A [cabal file] is provided.  If you would like to use cabal sandboxes,
install [llcheck] like this:

	cabal sandbox init
	cabal sandbox add-source ../../llcheck  # or other folder
	cabal install --only-dependencies


If you are not using sandboxes, install [llcheck] like this:

	cd path/to/llcheck
	cabal install


You can build then run one of the provided benchmarks:

	cabal build
	cabal bench example-name


### Via GHC Directly

Supposing llcheck and fitspec have been cloned in the same directory, you can
follow the following steps, to run a benchmark example:

	$ ls
	fitspec llcheck
	$ cd fitspec/black/bench
	$ ghc -i..:../../../llcheck example-benchmark.hs
	$ ./example-benchmark
	...
	...


Some of the benchmarks have command line parameters:
use `--help` for instructions.


Using FitSpec to guide property creation
----------------------------------------

TODO



Using FitSpec to analyse minimality and completeness of property-sets
---------------------------------------------------------------------

TODO



[llcheck]: https://github.com/rudymatela/llcheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs
[pretty]: https://hackage.haskell.org/package/pretty
[cabal file]: fitspec.cabal
