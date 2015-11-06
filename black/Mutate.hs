-- Enumeration of function mutations
module Mutate
  ( Mutable (..)
  , lsMutantsEq
  , mutantsIntegral
  )
where

import Test.Check
import Test.Check.Utils
import Data.List (intercalate, delete)
import Data.Maybe
import Utils (errorToNothing)

-- The first mutant returned by szMutants and mutants is the actual function
-- without mutation.
class Mutable a where
  lsMutants :: a -> [[a]]
  mutants :: a -> [a]
  lsMutants = map (:[]) . mutants
  mutants = concat . lsMutants

-- Beware: if the underlying enumeration for argument/return values generates
-- repeated elements there will be repeated and potentially null mutants.
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  lsMutants f = lsmap (defaultFunPairsToFunction f)
              $ lsConcatMap (`associations'` mutantsFor)
              $ lsCrescListsOf listing
    where mutantsFor x = case errorToNothing (f x) of
                           Nothing -> [[]]
                           Just fx -> tail (lsMutants fx)

lsdelete :: Eq a => a -> [[a]] -> [[a]]
lsdelete x = map (delete x)

-- TODO: Possible Optimization: (deleteOnce x)
lsMutantsEq :: (Listable a, Eq a) => a -> [[a]]
lsMutantsEq x = [x] : lsdelete x listing

-- Mutants of an Integral value.  Always start towards zero.  Alternating
-- between sucessor and predecessor.
-- The tail usage is there to avoid generating out of bound values.
-- (x-1) is usually safe though.
mutantsIntegral :: Integral a => a -> [a]
mutantsIntegral i | i > 0     = [i..] \/ tail [i,(i-1)..]
                  | otherwise = [i,(i-1)..] \/ tail [i..]

instance Mutable ()   where lsMutants = lsMutantsEq
--instance Mutable Int  where mutants = mutantsIntegral
instance Mutable Int  where lsMutants = lsMutantsEq
instance Mutable Char where lsMutants = lsMutantsEq
instance Mutable Bool where lsMutants = lsMutantsEq
instance (Eq a, Listable a) => Mutable [a]       where lsMutants = lsMutantsEq
instance (Eq a, Listable a) => Mutable (Maybe a) where lsMutants = lsMutantsEq

{- Alternative implementation for Mutable lists:
instance (Listable a, Mutable a) => Mutable [a]
  where lsMutants []     = [ [] ]
                         : [ ]
                         : tail listing
        lsMutants (x:xs) = [ (x:xs) ]
                         : [ [] ]
                         : tail (lsProductWith (:) (lsMutants x) (lsMutants xs))
-- -}

instance (Mutable a, Mutable b) => Mutable (a,b) where
  lsMutants (f,g) = lsMutants f `lsProduct` lsMutants g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  lsMutants (f,g,h) = lsProductWith (\f' (g',h') -> (f',g',h')) (lsMutants f) (lsMutants (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d) => Mutable (a,b,c,d) where
  lsMutants (f,g,h,i) = lsProductWith (\f' (g',h',i') -> (f',g',h',i')) (lsMutants f) (lsMutants (g,h,i))
