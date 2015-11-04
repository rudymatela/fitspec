-- Enumeration of function mutations
module Mutate
  ( module Mutate.Show
  , Mutable (..)
  , lsMutantsEq
  )
where

import Test.Check
import Test.Check.Utils
import Data.List (intercalate, delete)
import Data.Maybe
import Utils (errorToNothing)
import Mutate.Show

-- The first mutant returned by szMutants and mutants is the actual function
-- without mutation.
class Mutable a where
  szMutants :: a -> [[a]]
  mutants :: a -> [a]
  szMutants = map (:[]) . mutants
  mutants = concat . szMutants

-- Beware: if the underlying enumeration for argument/return values generates
-- repeated elements there will be repeated and potentially null mutants.
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  szMutants f = lsmap (defaultFunPairsToFunction f)
              $ lsConcatMap (`associations'` mutantsFor)
              $ lsCrescListsOf listing
    where mutantsFor x = case errorToNothing (f x) of
                           Nothing -> [[]]
                           Just fx -> tail (szMutants fx)

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

{- Alternative implementation for Mutable lists:
instance (Listable a, Mutable a) => Mutable [a]
  where szMutants []     = [ [] ]
                         : [ ]
                         : tail listing
        szMutants (x:xs) = [ (x:xs) ]
                         : [ [] ]
                         : tail (lsProductWith (:) (szMutants x) (szMutants xs))
-- -}

instance (Mutable a, Mutable b) => Mutable (a,b) where
  szMutants (f,g) = szMutants f `lsProduct` szMutants g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  szMutants (f,g,h) = lsProductWith (\f' (g',h') -> (f',g',h')) (szMutants f) (szMutants (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d) => Mutable (a,b,c,d) where
  szMutants (f,g,h,i) = lsProductWith (\f' (g',h',i') -> (f',g',h',i')) (szMutants f) (szMutants (g,h,i))
