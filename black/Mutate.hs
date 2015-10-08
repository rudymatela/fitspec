-- Enumeration of function mutations
module Mutate
  ( module Mutate.Show
  , Mutable (..)
  )
where

import Test.Check
import Test.Check.Utils
import Data.List (intercalate, delete)
import Data.Maybe
import Table
import Utils (errorToNothing)
import Mutate.Show

-- The first mutant returned by szMutants and mutants is the actual function
-- without mutation.
class Mutable a where
  szMutants :: a -> [[a]]
  mutants :: a -> [a]
  szMutants = map (:[]) . mutants
  mutants = concat . szMutants

{-
-- TODO: Make a test suite comparing both impls??
-- Here canonicalMutation instead of proper mutation is being used: this is
-- necessary when making the product of several mutations (in a tuple of
-- functions there is need to mutate a single element).
instance (Eq a, Eq b, Listable a, Listable b) => Mutable (a -> b) where
  szMutants f = lsmap (defaultFunPairsToFunction f)
              $ lsfilter (canonicalMutation f)
              $ lsFunctionPairs listing listing
-}

instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  szMutants f = lsmap (defaultFunPairsToFunction f)
              $ lsConcatMap (\as -> associations' as (tail . szMutants . f))
              $ lsCrescListsOf listing


lsdelete :: Eq a => a -> [[a]] -> [[a]]
lsdelete x = map (delete x)

-- TODO: Possible Optimization: (deleteOnce x)
lsMutantsEq :: (Listable a, Eq a) => a -> [[a]]
lsMutantsEq x = [x] : lsdelete x listing

instance Mutable ()   where szMutants = lsMutantsEq
instance Mutable Int  where szMutants = lsMutantsEq
instance Mutable Char where szMutants = lsMutantsEq
instance Mutable Bool where szMutants = lsMutantsEq
instance (Eq a, Listable a) => Mutable [a]       where szMutants = lsMutantsEq
instance (Eq a, Listable a) => Mutable (Maybe a) where szMutants = lsMutantsEq

instance (Mutable a, Mutable b) => Mutable (a,b) where
  szMutants (f,g) = szMutants f `lsProduct` szMutants g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  szMutants (f,g,h) = lsProductWith (\f' (g',h') -> (f',g',h')) (szMutants f) (szMutants (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d) => Mutable (a,b,c,d) where
  szMutants (f,g,h,i) = lsProductWith (\f' (g',h',i') -> (f',g',h',i')) (szMutants f) (szMutants (g,h,i))

canonicalMutation :: Eq b => (a -> b) -> [(a, b)] -> Bool
-- This simple version on the line below
-- is one that does not deal with partially undefined functions.
-- canonicalMutation f = all (\(a,r) -> f a /= r)
canonicalMutation f = all different
  where
    -- the errorToNothing here deals partial functions (error/undefined)
    -- We define that mutating undefined values is noncanonical
    different (a,r) = case errorToNothing $ f a of
                        Just r' -> r' /= r
                        Nothing -> False -- for our purposes,
                                         -- undefined is equal to anything
