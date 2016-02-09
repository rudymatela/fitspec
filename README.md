FitSpec
=======

FitSpec assists you in refining property-sets for testing Haskell programs.
FitSpec does black-box mutation testing and offers guidance towards
a minimal and complete property-set.

**This a work in progress: currently in early stage**


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


Clone FitSpec, then run an example benchmark, e.g.:

	$ git clone git@github.com:rudymatela/fitspec
	$ cd fitspec
	$ cabal bench sorting


The drawback of this solution is that everytime there is an update in llcheck,
you have to reinstall it after pulling.


### Via Cabal, with sandboxes

*Cabal > 1.18 is needed*

Clone llcheck and fitspec and enter fitspec folder:

	$ git clone git@github.com:rudymatela/llcheck
	$ git clone git@github.com:rudymatela/fitspec
	$ cd fitspec


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


If you want to compile a file that uses FitSpec on GHC, you do:

	$ ghc -ipath/to/llcheck:path/to/fitspec file.hs


**Example:** suppose llcheck and fitspec have been cloned in the same
directory, do this to run the sorting example benchmark:

	$ ls
	fitspec llcheck
	$ cd fitspec/bench
	$ ghc -i..:../../llcheck sorting.hs
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
We can use FitSpec to guide property creation.


We first import what is needed:

	import FitSpec
	import Data.List (sort)


Then we need a property list function: given a sorting implementation, return
the properties applied to *that* implementation.  Since we don't have any
properties, we will start by returning and empty list:

	properties :: (Show a, Ord a, Listable a)
	           => ([a] -> [a]) -> [Properties]
	properties sort' =
	  []


Then, we need a main function, that calls the FitSpec's `report` function,
which will report the results of mutation testing.
It needs a function to be mutated and the property list.

	main = report (sort::[Int]->[Int]) properties

Optionally, for a nicer output, you might want to use the reportWith function,
which allows specifying function and argument names (among other options):

	main = reportWith args { callNames = ["sort xs"] }
	                  (sort::[Int]->[Int]) properties

By having the three sections above in a file called sorting.hs,
we then compile and run:

	$ ghc -ipath/to/llcheck:path/to/fitspec sorting.hs
	[9 of 9] Compiling Main             ( sorting.hs, sorting.o )
	Linking sorting ...

	$ ./sorting
	Results based on at most 4000 test cases for each of 2000 mutant variations.

	Property   #Survivors    Smallest or simplest
	 sets       (%Killed)     surviving mutant

	[]         2000 (0%)     \xs -> case xs of
	                                  [] -> [0]
	                                  _ -> sort xs

The output is self-explanatory.  Obviously, our empty property set `[]` did not
kill any mutant (`0%`).  In other words, all of the `2000` mutants survived.
(The actual number of mutants tested will vary depending on your machine, it
will probably be higher than 2000 *in this case*, by default FitSpec runs for
at least 5 seconds.)

The surviving mutant shown on the third column is clearly not a valid
implementation of sort.  For the empty list, it returns `[0]`.  We should
improve our property set by killing that mutant.  Lets start very simple by
adding a property stating that sorting an empty list must yield an empty list:

	properties sort' =
	  [ property $ sort' [] == []
	  ]

Above, we need to apply the function `property` to each property in the list.
Now:

	$ ./sorting
	Results based on at most 4000 test cases for each of 2000 mutant variations.

	Property   #Survivors    Smallest or simplest
	 sets       (%Killed)     surviving mutant

	[1]        984 (49%)     \xs -> case xs of
	                                  [0] -> []
	                                  _ -> sort xs

	[]         2000 (0%)     \xs -> case xs of
	                                  [] -> [0]
	                                  _ -> sort xs

The last row of results is the same as before (all mutants still obviously
survive the empty property set).  The *first row* show that there are `984`
*surviving mutants* (`49%`) for the first property `[1]`: the smallest one is
shown on the third column.  It sorts `[0]` to `[]`, which is not valid.  Lets
still be very simple -- sorting a list with one value must yield a list with
the same value:

	properties sort' =
	  [ property $        sort' [] == []
	  , property $ \x -> sort' [x] == [x]
	  ]

Note that, our new property (2) has a free variable.  Now:

	$ ./sorting
	Results based on at most 1000 test cases for each of 500 mutant variations.

	Property   #Survivors   Smallest or simplest
	 sets       (%Killed)    surviving mutant

	[1,2]      134 (73%)    \xs -> case xs of
	                                 [0,0] -> []
	                                 _ -> sort xs
	...

Only 27% of mutants to go, perhaps a property stating that the length of the
sorted list should not change?

	properties sort' =
	  [ property $                 sort' [] == []
	  , property $ \x  ->         sort' [x] == [x]
	  , property $ \xs -> length (sort' xs) == length xs
	  ]

Now:

	$ ./sorting
	Results based on at most 1000 test cases for each of 500 mutant variations.

	Property   #Survivors   Smallest or simplest
	 sets       (%Killed)    surviving mutant

	[2,3]      12 (97%)     \xs -> case xs of
	                                 [0,0] -> [0,1]
	                                 _ -> sort xs
	...

	Conjectures based on at most 1000 test cases for each of 500 mutant variations:
	[3] ==> [1]     95% killed (likely)

The first row show that the current candidate minimal-complete propety-set
kills all but `4` mutants and is composed only by properties 2 and 3 (`[2,3]`).
When possible, FitSpec also reports *conjectures* based on test results.  In
this case, that property `sort [] == []` (1) follows from the length property
(3).  Since that is *clearly* true, we can safely remove that property.

	properties sort' =
	  [ property $ \x    ->         sort' [x] == [x]
	  , property $ \xs   -> length (sort' xs) == length xs
	  , property $ \x xs -> elem x (sort' xs) == elem x xs
	  ]

Now:

	$ ./sorting
	Property   #Survivors   Smallest or simplest
	 sets       (%Killed)    surviving mutant

	 [2,3]      2 (99%)      \xs -> case xs of
	                                  [0,1] -> [1,0]
	                                  _ -> sort xs
	...
	Conjectures based on at most 1000 test cases for each of 500 mutant variations:
	[2,3] ==> [1]     99% killed (possible+)

We could go on, but *at this point, you probably got how it works*.  As an
exercise you can try to improve our property-set over `sort` by killing the
above mutant by adding a new property.  Later, you can try to improve the
results by increasing the time limit (`minimumTime = 10` on args).


### To analyse minimality and completeness of property-sets

If you are new to the tool,
I recommend reading the [previous section](#to-guide-property-creation)
of this README file.

Then, just look at one of the [example programs on the bench folder](bench).


Important modules
-----------------

* [FitSpec](FitSpec.hs):
  the entry point, import this to use FitSpec;

* [FitSpec.Engine](FitSpec/Engine.hs):
  main engine that does calculations and generate reports;

* [FitSpec.Mutable](FitSpec/Mutable.hs):
  list mutations of a given function without repetitions;

* [FitSpec.ShowMutable](FitSpec/ShowMutable.hs):
  show mutations;

* [example benchmarks](bench):
  example use cases for FitSpec,
  some are customizable using command line arguments
  (sorting, booleans, lists, pretty-printing, etc).


The modules FitSpec.Grey refer to the "grey-box" version, that does mutant
classification.  Normal FitSpec is simpler, you should probably start with it.


[llcheck]: https://github.com/rudymatela/llcheck
[cmdargs]: https://hackage.haskell.org/package/cmdargs
[pretty]: https://hackage.haskell.org/package/pretty
[cabal file]: fitspec.cabal
