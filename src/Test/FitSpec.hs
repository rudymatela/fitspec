-- |
-- Module      : Test.FitSpec
-- Copyright   : (c) 2015-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- __ FitSpec: refining property-sets for functional testing __
--
-- FitSpec provides automated assistance in the task of refining test properties
-- for Haskell functions.  FitSpec tests mutant variations of functions under test
-- against a given property set, recording any surviving mutants that pass all
-- tests.  FitSpec then reports:
--
-- * /surviving mutants:/
--   indicating incompleteness of properties,
--   prompting the user to amend a property or to add a new one;
-- * /conjectures:/
--   indicating redundancy in the property set,
--   prompting the user to remove properties so to reduce the cost of testing.
--
-- Example, refining a @sort@ specification:
--
-- > import Test.FitSpec
-- > import Data.List (sort)
-- >
-- > properties sort =
-- >   [ property $ \xs   -> ordered (sort xs)
-- >   , property $ \xs   -> length (sort xs) == length xs
-- >   , property $ \x xs -> elem x (sort xs) == elem x xs
-- >   , property $ \x xs -> notElem x (sort xs) == notElem x xs
-- >   , property $ \x xs -> minimum (x:xs) == head (sort (x:xs))
-- >   ]
-- >   where
-- >   ordered (x:y:xs) = x <= y && ordered (y:xs)
-- >   ordered _        = True
-- >
-- > main = mainWith args { names = ["sort xs"]
-- >                      , nMutants = 4000
-- >                      , nTests   = 4000
-- >                      , timeout  = 0
-- >                      }
-- >                 (sort::[Word2]->[Word2])
-- >                 properties
--
-- The above program reports the following:
--
-- > Apparent incomplete and non-minimal specification based on
-- > 4000 test cases for each of properties 1, 2, 3, 4 and 5
-- > for each of 4000 mutant variations.
-- >
-- > 3 survivors (99% killed), smallest:
-- >   \xs -> case xs of
-- >            [0,0,1] -> [0,1,1]
-- >            _ -> sort xs
-- >
-- > apparent minimal property subsets:  {1,2,3} {1,2,4}
-- > conjectures:  {3}    =  {4}     96% killed (weak)
-- >               {1,3} ==> {5}     98% killed (weak)
module Test.FitSpec
  (
  -- * Encoding properties
    Property
  , property

  -- * Configuring reports
  , Args (..), ShowMutantAs (..)
  , args
  , fixargs

  -- * Reporting results
  , report
  , reportWith
  , reportWithExtra

  -- ** Parsing command line arguments
  , mainWith
  , defaultMain
  , getArgs
  , getArgsWith

  -- * Mutable types
  , Mutable (..)
  , mutiersEq

  , ShowMutable (..)
  , mutantSEq
  , showMutantAsTuple
  , showMutantDefinition
  , showMutantNested
  , showMutantBindings

  -- * Automatic derivation
  , deriveMutable
  , deriveMutableE
  , deriveMutableCascading
  , deriveMutableCascadingE

  -- * Re-export modules
  , module Test.FitSpec.TestTypes
  , module Test.LeanCheck
  )
where

import Test.FitSpec.Engine
import Test.FitSpec.Report
import Test.FitSpec.Main

import Test.FitSpec.Mutable
import Test.FitSpec.Mutable.Tuples
import Test.FitSpec.ShowMutable
import Test.FitSpec.ShowMutable.Tuples
import Test.FitSpec.Derive
import Test.FitSpec.TestTypes

import Test.LeanCheck
