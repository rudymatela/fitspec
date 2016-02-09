-- | FitSpec: refining property-sets for functional testing
--
-- Example, properties over 'not':
--
-- > import FitSpec
-- >
-- > properties :: (Bool -> Bool) -> [Bool]
-- > properties not =
-- >   [ property $ \p -> not (not p) == p
-- >   , property $ \p -> not p /= p
-- >   , property $       not True == False
-- >   ]
-- >
-- > main = report not properties
--
-- FitSpec reports:
--
-- > Results based on at most 2 test cases
-- >     for each of 3 mutant variations.
-- >
-- > Property    #Survivors   Smallest or simplest
-- >  sets        (%Killed)    surviving mutant
-- >
-- > [2] [1,3]   0 (100%)
-- >
-- > [1]         1 (66%)      \p -> case p of
-- >                                  False -> False
-- >                                  True -> True
-- >                                  _ -> not p
-- >
-- > [3]         1 (66%)      \p -> case p of
-- >                                  False -> False
-- >                                  _ -> not p
-- >
-- > Conjectures based on at most 2 test cases
-- >         for each of 3 mutant variations:
-- > [2]  =  [1,3]     100% killed (very weak)
-- >
-- > Tests and mutants were exhausted,
-- > results are known to be correct.
--
-- FitSpec was tested on GHC 7.10, 7.8, 7.6 and 7.4.
module FitSpec
  ( module FitSpec.Engine
  , module FitSpec.Main

  , module FitSpec.Mutable
  , module FitSpec.ShowMutable
  , module FitSpec.Derive
  , module FitSpec.TestTypes

  , module Test.Check
  )
where
-- TODO: Explictly export *only* needed functions

import FitSpec.Engine
import FitSpec.Main

import FitSpec.Mutable
import FitSpec.ShowMutable hiding (showMutantN)
import FitSpec.Derive hiding (showMutantN)
import FitSpec.TestTypes

import Test.Check
