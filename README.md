FitSpec
=======

Find minimal and complete sub-sets of properties
describing a Haskell program
using black-box mutation testing.

**This a work in progress: currently in very early stage**

Four different versions are being developed in parallel:

* black-box functions under test, black-box properties:
  see the [black](black) folder;

* grey-box functions under test, grey-box properties:
  see also the [black](black) folder (`FitSpec.Grey` file);

* black-box functions under test, white-box properties -- mutation by demand:
  see the [white](white) folder;

* mutation via patterns:
  see the [patterns](patterns) folder.

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


Tutorial
--------

For a tutorial on using the black-box version, see the [README
file](black/README.md) on the [black folder](black).

[llcheck]: https://github.com/rudymatela/llcheck
