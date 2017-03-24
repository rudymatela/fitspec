FitSpec
=======

[![FitSpec Build Status][build-status]][build-log]
[![FitSpec on Hackage][hackage-version]][fitspec-on-hackage]

FitSpec provides automated assistance in the task of refining test properties
for Haskell functions.  FitSpec tests mutant variations of functions under test
against a given property set, recording any surviving mutants that pass all
tests.  FitSpec then reports:

* *surviving mutants:*
  indicating incompleteness of properties,
  prompting the user to amend a property or to add a new one;
* *conjectures:*
  indicating redundancy in the property set,
  prompting the user to remove properties so to reduce the cost of testing.

Installing FitSpec
------------------

To install the latest FitSpec version from Hackage, just:

    $ cabal install fitspec

Pre-requisites are [cmdargs] and [leancheck].
They should be automatically resolved and installed by [Cabal].


Using FitSpec
-------------

As an example, consider the following properties describing a `sort` function:

    prop_ordered xs = ordered (sort xs)
    prop_length xs = length (sort xs) == length xs
    prop_elem x xs = elem x (sort xs) == elem x xs
    prop_notElem x xs = notElem x (sort xs) == notElem x xs
    prop_min x xs = head (sort (x:xs)) == minimum (x:xs)

We provide the above properties to FitSpec in the following program:

    import Test.FitSpec
    import Data.List

    properties sort =
      [ property $ \xs -> ordered (sort xs)
      , property $ \xs -> length (sort xs) == length xs
      , property $ \x xs -> elem x (sort xs) == elem x xs
      , property $ \x xs -> notElem x (sort xs) == notElem x xs
      , property $ \x xs -> head (sort (x:xs)) == minimum (x:xs)
      ]
      where
      ordered (x:y:xs) = x <= y && ordered (y:xs)
      ordered _        = True

    main = mainWith args { names = ["sort xs"]
                         , nMutants = 4000
                         , nTests = 4000
                         , timeout = 0
                         }
                    (sort::[Word2]->[Word2])
                    properties

The above program reports, after a few seconds, that our property set is
apparently *neither minimal nor complete*.

    $ ./fitspec-sort
    Apparent incomplete and non-minimal specification based on
    4000 test cases for each of properties 1, 2, 3, 4 and 5
    for each of 4000 mutant variations.

    3 survivors (99% killed), smallest:
      \xs -> case xs of
               [0,0,1] -> [0,1,1]
               _ -> sort xs

    apparent minimal property subsets:  {1,2,3} {1,2,4}
    conjectures:  {3}    =  {4}     96% killed (weak)
                  {1,3} ==> {5}     98% killed (weak)

*Completeness:* Of 4000 mutants, 3 survive testing against our 5 properties.
The surviving mutant is clearly not a valid implementation of `sort`, but
indeed satisfies those properties.  As a specification, the property set is
*incomplete* as it omits to require that sorting preserves the number of
occurrences of each element value: `\x xs -> count x (sort xs) == count x xs`

*Minimality:*
So far as testing has revealed, properties 3 and 4 are equivalent and property
5 follows from 1 and 3 (conjectures).  It is *up to the user* to check whether
these conjectures are true.  Indeed they are, so in future testing we could
safely omit properties 4 and 5.

*Refinement:* If we omit redundant properties, and add a property to kill the
surviving mutant, our refined properties are:

    properties sort =
      [ \xs ->   ordered (sort xs)
      , \xs ->    length (sort xs) == length xs
      , \x xs ->  elem x (sort xs) == elem x xs
      , \x xs -> count x (sort xs) == count x xs
      ]

(The implementation of `count` is left as an exercise to the reader.)

FitSpec now reports:

    Apparent complete but non-minimal specification based on
    4000 test cases for each of properties 1, 2, 3 and 4
    for each of 4000 mutant variations.

    0 survivors (100% killed).

    apparent minimal property subsets:  {1,4}
    conjectures:  {4} ==> {2,3}     99% killed (weak)

As reported, properties 2 and 3 are implied by property 4, since that is true,
we can safely remove properties 2 and 3 to arrive at a minimal and complete
propety set.


### User-defined datatypes

If you want to use FitSpec to analyse functions over user-defined datatypes,
those datatypes should be made instances of the [Listable], [Mutable] and
[ShowMutable] typeclasses.  Check the Haddock documentation of each class for
how to define instances manually.  If datatypes do not follow a data invariant,
instances can be automatically derived using [TH] by:

    deriveMutable ''DataType


More documentation
------------------

For more examples, see the [eg](eg) and [bench](bench) folders.

For further documentation, consult the [doc](doc) folder and [FitSpec API]
documentation on Hackage.

FitSpec has been subject to a paper, see the
[FitSpec paper on Haskell Symposium 2016](https://matela.com.br/paper/fitspec.pdf)

[Listable]:    https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[Mutable]:     https://hackage.haskell.org/package/fitspec/docs/Test-FitSpec.html#t:Mutable
[ShowMutable]: https://hackage.haskell.org/package/fitspec/docs/Test-FitSpec.html#t:ShowMutable
[FitSpec API]: https://hackage.haskell.org/package/fitspec/docs/Test-FitSpec.html

[leancheck]: https://hackage.haskell.org/package/leancheck
[cmdargs]:   https://hackage.haskell.org/package/cmdargs
[pretty]:    https://hackage.haskell.org/package/pretty

[TH]:    https://wiki.haskell.org/Template_Haskell
[Cabal]: https://www.haskell.org/cabal

[build-status]: https://travis-ci.org/rudymatela/fitspec.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/fitspec
[hackage-version]: https://img.shields.io/hackage/v/fitspec.svg
[fitspec-on-hackage]: https://hackage.haskell.org/package/fitspec
