FitSpec
=======

Find minimal and complete sub-sets of properties
describing a Haskell program
using black-box mutation testing.

**This a work in progress: currently in very early stage**

Two versions are being developed in parallel
(I haven't decided which is the best yet):

* black-box functions under test, black-box properties: see the [black](black) folder;
* black-box functions under test, white-box properties -- mutation by demand: see the [white](white) folder.

See each respective folder.

Running
-------

You'll need [llcheck] to run this.  To run using cabal sandboxes:

	cd black   # or: cd white
	cabal sandbox init
	cabal sandbox add-source path/to/llcheck
	cabal install --only-dependencies
	cabal build
	cabal bench example-name

[llcheck]: https://github.com/rudymatela/llcheck
