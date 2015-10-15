FitSpec
=======

Find minimal and complete sub-sets of properties
describing a Haskell program
using black-box mutation testing.

**This a work in progress: currently in very early stage**


Compiling and Running
---------------------

You can compile and run in several different ways.
This README provides a few options:

* via Cabal, without sandboxes (*for now, not recommended*)
* via Cabal, with sandboxes (recommended)
* via GHC directly (recommended)

For details, see each subsection.

### Via Cabal, without sandboxes

If needed, install [cmdargs] and [pretty]:

	$ cabal install cmdargs pretty


Clone then install llcheck:

	$ git clone git@github.com:rudymatela/llcheck
	$ cd llcheck
	$ cabal install


Clone then run a FitSpec example benchmark, in this case, sorting:

	$ git clone git@github.com:rudymatela/fitspec
	$ cd fitspec/black
	$ cabal bench sorting


The drawback of this solution is that everytime there is an update in llcheck,
you have to reinstall it after pulling.


### Via Cabal, with sandboxes

*Cabal > 1.18 is needed*

Clone llcheck and fitspec

	$ git clone git@github.com:rudymatela/llcheck
	$ git clone git@github.com:rudymatela/fitspec


Enter FitSpec black-box version folder:

	$ cd fitspec/black


Initialize a sandbox, add llcheck as a source, then install dependencies:

	$ cabal sandbox init
	$ cabal sandbox add-source ../../llcheck
	$ cabal install --only-dependencies


Run the sorting example:

	$ cabal bench sorting


An advantage of this solution is that llcheck is recompiled automatically if
there is an update.

To create a new benchmark, you should create a file in the bench folder and
create a new section in the [cabal file].


### Via GHC directly

*If you prefer to use cabal, see the previous sections.*


If needed, install [cmdargs] and [pretty]:

	$ cabal install cmdargs pretty


Clone both llcheck and fitspec:

	$ git clone git@github.com:rudymatela/llcheck
	$ git clone git@github.com:rudymatela/fitspec


The general rule, if you want to compile a file that uses FitSpec on GHC, you do:

	$ ghc -ipath/to/llcheck:path/to/fitspec file.hs


**Example:** suppose llcheck and fitspec have been cloned in the same
directory, do this to run the sorting example benchmark:

	$ ls
	fitspec llcheck
	$ cd fitspec/black/bench
	$ ghc -i..:../../../llcheck sorting.hs
	$ ./sorting
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
	[]   100   \xs -> case xs of
	                    [] -> [0]
	                    _ -> sort xs

What does that output mean?

* The first column (`[]`)
  indicates our empty property set
* The second column  (`100`)
  indicates the number of surviving mutants
* The third column  (`\xs -> ...`)
  shows the smallest surviving mutant for our empty property set

The surviving mutant shown, is clearly not a valid implementation of sort.  For
the empty list, it returns `[0]`.  We should improve our property set by
killing that mutant.  Lets start very simple: sorting an empty list must yield
an empty list:

	pmap n sort' =
	  [ sort' [] == []
	  ]

Now:

	$ ./sorting
	[1]   52    \xs -> case xs of
	                     [0] -> []
	                     _ -> sort xs

	[]    100   \xs -> case xs of
	                     [] -> [0]
	                     _ -> sort xs

The last lines are the same as before (all mutants obviously survive the empty
property set).  The *first lines* show that there are `52` *surviving mutants*
for the first property `[1]`: the smallest one is shown on the third column.
It sorts `[0]` to `[]`, which is not valid.  Lets still be very simple --
sorting a list with one value must yield a list with the same value:

	pmap n sort' =
	  [                  sort' [] == []
	  , holds n $ \x -> sort' [x] == [x]
	  ]

Note that, our new property (2) has a free variable, that is why `holds` is
needed: it checks whether a property holds for a given number of tests (`n`)
Now:

	$ ./sorting
	[1,2]    25   \xs -> case xs of
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
	[2,3]    4    \xs -> case xs of
	                       [0,0] -> [0,1]
	                       _ -> sort xs
	...

The first lines show that the current candidate minimal-complete propety-set
kills all but `4` mutants and is composed only by properties 2 and 3 (`[2,3]`).
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
and 3.  But, it is not over!  We can now update our `reportWith` call to check
more mutants (e.g.: `500` instead of `100`).  After that:

	$ ./sorting
	[2,3]    2    \xs -> case xs of
	                       [0,1] -> [1,0]
	                       _ -> sort xs
	...

We could go on, but *at this point, you got the point*.  As an exercise you can
try to improve our property-set over `sort` by killing the above mutant by
adding a new property.


### To analyse minimality and completeness of property-sets

If you are new to the tool,
I recommend reading the [previous section](#to-guide-property-creation)
of this README file.

Then, just look at one of the [example programs on the bench folder](bench).


Important modules
-----------------

* [FitSpec](FitSpec.hs):
  the entry point, implements the calculations behind minimality and completeness

* [Mutate](Mutate.hs):
  list mutations of a given function without repetitions

* [Mutate.Show](Mutate/Show.hs):
  show mutations

* [example benchmarks](bench):
  example use cases for FitSpec,
  some are customizable using command line arguments
  (sorting, booleans, lists, pretty-printing, etc)

* [Utils](Utils.hs):
  miscellaneous utility functions


The modules FitSpecC and Mutation refer to the "grey-box" version, that does
mutant classification.  Normal FitSpec is simpler, you should probably start
with it.


[llcheck]: https://github.com/rudymatela/llcheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs
[pretty]: https://hackage.haskell.org/package/pretty
[cabal file]: fitspec.cabal
