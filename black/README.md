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


Using FitSpec
-------------

### To guide property creation

Suppose we want to write properties for the function sort,
but we do not know where to start.
We can use FitSpec to guide property generation.


We first import what is needed:

	import Test.Check
	import FitSpec
	import Data.List (sort)


Then we need a property map: given the number of tests and a sorting
implementation, return whether each property holds.  Since we don't have any
properties, we will start by returning and empty list:

	pmap :: (Show a, Ord a, Listable a)
	     => Int -> ([a] -> [a]) -> [Bool]
	pmap n sort' =
	  []


Then, we need a main function, that calls the FitSpec's `reportWith` function,
which will report the results of mutation testing.  It needs a callName
template, the number of mutants, a function to be mutated and the property map.

	main =
	  reportWith
	    (args {callNames = ["sort xs"]}) -- set standard call name
	    100                              -- number of mutants
	    (sort::[Int]->[Int])             -- function under test
	    (pmap 1000)                      -- property-map (1000 tests)


By having the three sections above in a file called sorting.hs,
we then compile and run:

	$ ghc -ipath/to/llcheck:path/to/fitspec/black sorting.hs
	[9 of 9] Compiling Main             ( sorting.hs, sorting.o )
	Linking sorting ...

	$ ./sorting
	100   []   \xs -> case xs of
	                    [] -> [0]
	                    _ -> sort xs

What does that output mean?

* The first column  (`100`)
  indicates the number of surviving mutants
* The second column (`[]`)
  indicates our empty property set
* The third column  (`\xs -> ...`)
  shows the smallest surviving mutant for our empty property set

The surviving mutant shown, is clearly not a valid implementation of sort.  For
the empty list, it returns `[0]`.  We should improve our property set by killing
that mutant, lets start very simple: sorting an empty list must yield an empty
list:

	pmap n sort' =
	  [ sort' [] == []
	  ]

Now:

	$ ./sorting
	52    [1]   \xs -> case xs of
	                     [0] -> []
	                     _ -> sort xs

	100   []    \xs -> case xs of
	                     [] -> [0]
	                     _ -> sort xs

The last lines are the same as before (all mutants obviously survive the empty
property set).  The *first lines* show that there are *52 surviving mutants*
for the first property `[1]`: the smallest one is shown on the third column.
It sorts `[0]` to `[]`, which is not valid.  Lets still be very simple --
sorting a list with one value must yield a list with the same value:

	pmap n sort' =
	  [                  sort' [] == []
	  , holds n $ \x -> sort' [x] == [x]
	  ]

Note that our new property (2), has a free variable, that is why `holds` is
needed: it checks whether a property holds for a given number of tests: we use
one of the parameters of our property set, `n`.  Now:

	$ ./sorting
	25    [1,2]   \xs -> case xs of
	                       [0,0] -> []
	                       _ -> sort xs
	...

Only 25 mutants to go, perhaps a property stating that the length of the sorted
list should not change?

	pmap n sort' =
	  [                           sort' [] == []
	  , holds n $ \x  ->         sort' [x] == [x]
	  , holds n $ \xs -> length (sort' xs) == length xs
	  ]

Now:

	$ ./sorting
	4     [2,3]   \xs -> case xs of
	                       [0,0] -> [0,1]
	                       _ -> sort xs
	...

The first lines show that the current candidate minimal-complete propety-set
kills all but 4 mutants and is composed only by properties 2 and 3 (`[2,3]`).
We can safely remove property `sort' [] == []` as it is not needed (it follows
from the length property).  We should also add a property to kill that mutant:
elements of the list should not change.

	pmap n sort' =
	  [ holds n $ \x    ->         sort' [x] == [x]
	  , holds n $ \xs   -> length (sort' xs) == length xs
	  , holds n $ \x xs -> elem x (sort' xs) == elem x xs
	  ]

Now:

	$ ./sorting
	0     [2,3]
	...

The first line shows that, now, there are no surviving mutants for properties 2
and 3.  But, it is not over, we can now update our `reportWith` call to check
more mutants (e.g.: 500 instead of 100).  After that:

	$ ./sorting
	2     [2,3]   \xs -> case xs of
	                       [0,1] -> [1,0]
	                       _ -> sort xs
	...

I could go on, but at this point you get the point.  As an exercise you can try
to improve our property-set over `sort` by killing the above mutant.


### To analyse minimality and completeness of property-sets

TODO



[llcheck]: https://github.com/rudymatela/llcheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs
[pretty]: https://hackage.haskell.org/package/pretty
[cabal file]: fitspec.cabal
