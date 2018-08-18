-- |
-- Module      : Test.FitSpec.Mutable
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Enumeration of function mutations
module Test.FitSpec.Mutable
  ( Mutable (..)
  , mutiersEq
--, mutantsIntegral
  )
where

import Test.LeanCheck
import Data.List (intercalate, delete)
import Data.Maybe
import Test.LeanCheck.Error (errorToNothing)
import Data.Ratio (Ratio)
import Data.Word (Word)

-- | This typeclass is similar to 'Listable'.
--
-- A type is 'Mutable' when there exists a function that
-- is able to list mutations of a value.
-- Ideally: list all possible values without repetitions.
--
-- Instances are usually defined by a 'mutiers' function that
-- given a value, returns tiers of mutants of that value:
--   the first  tier contains the equivalent mutant, of size 0,
--   the second tier contains mutants of size 1,
--   the third  tier contains mutants of size 2,
--   and so on.
--
-- The equivalent mutant is the actual function without mutations.
--
-- The size of a mutant is given by the sum of:
--   the number of mutated points (relations) and
--   the sizes of mutated arguments and results.
--
-- To get only inequivalent mutants,
-- just take the 'tail' of either 'mutants' or 'mutiers':
--
-- > tail mutants
--
-- > tail mutiers
--
-- Given that the underlying 'Listable' enumeration has no repetitions,
-- parametric instances defined in this file will have no repeated mutants.
class Mutable a where
  mutiers :: a -> [[a]]
  mutants :: a -> [a]
  mutiers = map (:[]) . mutants
  mutants = concat . mutiers
  {-# MINIMAL mutants | mutiers #-}


-- *** *** Instances for (non-functional) data types *** ***

-- | Implementation of 'mutiers' for non-functional data types.
-- Use this to create instances for user-defined data types, e.g.:
--
-- > instance MyData
-- >   where mutiers = mutiersEq
--
-- and for parametric datatypes:
--
-- > instance (Eq a, Eq b) => MyDt a b
-- >   where mutiers = mutiersEq
--
-- Examples:
--
-- > mutiersEq True = [[True], [False]]
-- > mutiersEq 2   = [[2], [0], [1], [], [3], [4], [5], [6], [7], [8], [9], ...]
-- > mutiersEq [1] = [[[1]], [[]], [[0]], [[0,0]], [[0,0,0],[0,1],[1,0],[-1]], ...]
mutiersEq :: (Listable a, Eq a) => a -> [[a]]
mutiersEq x = [x] : deleteT x tiers

-- | > mutants () = [()]
instance Mutable ()   where mutiers = mutiersEq

-- | > mutants 3 = [3,0,1,2,4,5,6,7,8,...]
instance Mutable Int  where mutiers = mutiersEq

instance Mutable Integer where mutiers = mutiersEq

instance Mutable Char where mutiers = mutiersEq

-- | > mutants True = [True,False]
instance Mutable Bool where mutiers = mutiersEq

-- | > mutants [0] = [ [0], [], [0,0], [1], ...
instance (Eq a, Listable a) => Mutable [a]       where mutiers = mutiersEq

-- | > mutants (Just 0) = [Just 0, Nothing, ...
instance (Eq a, Listable a) => Mutable (Maybe a) where mutiers = mutiersEq

instance (Eq a, Listable a, Eq b, Listable b) => Mutable (Either a b)
  where mutiers = mutiersEq

instance (Eq a, Listable a, Integral a) => Mutable (Ratio a)
  where mutiers = mutiersEq

instance Mutable Float    where mutiers = mutiersEq
instance Mutable Double   where mutiers = mutiersEq
instance Mutable Ordering where mutiers = mutiersEq
instance Mutable Word     where mutiers = mutiersEq

{- Alternative implementations for Mutable Ints and Lists.
-- These do not improve results significantly.
-- That is why I have kept the simpler mutations above.

-- |- Generate mutants of an Integral value.
-- Alternates between successors and predecessors of the original number.
-- The enumeration starts "towards" zero.
mutantsIntegral :: Integral a => a -> [a]
mutantsIntegral i | i > 0     = [i..] +| tail [i,(i-1)..]
                  | otherwise = [i,(i-1)..] +| tail [i..]
-- NOTE: tail is there to avoid generating out of bound values
--       as (i-1) is usually safe while (i-2) is not.

instance Mutable Int  where mutants = mutantsIntegral

instance (Listable a, Mutable a) => Mutable [a]
  where mutiers []     = [ [] ]
                       : [ ]
                       : tail tiers
        mutiers (x:xs) = [ (x:xs) ]
                       : [ [] ]
                       : tail (lsProductWith (:) (mutiers x) (mutiers xs))
-- -}


-- *** *** Instances for functional types *** ***

-- | Mutate a function at a single point.
-- The following two declarations are equivalent:
--
-- > id' = id `mut` (0,1)
--
-- > id' 0 = 1
-- > id' x = x
mut :: Eq a => (a -> b) -> (a,b) -> (a -> b)
mut f (x',fx') = \x -> if x == x'
                         then fx'
                         else f x

-- | Mutate a function at several points.
--
-- > f `mutate` [(x,a),(y,b),(z,c)] = f `mut` (x,a) `mut` (y,b) `mut` (z,c)
mutate :: Eq a => (a -> b) -> [(a,b)] -> (a -> b)
mutate f ms = foldr (flip mut) f ms -- or: foldl mut f ms

-- | Return tiers of possible mutations for a single point of a function.
-- If the function is undefined at that point, no mutations are provided.
-- This function does not return the null mutant.
--
-- > (+1) `mutationsFor` 1 = [ [(1,0)], [(1,1)], [], [(1,3)], [(1,4)], ...
mutationsFor :: Mutable b => (a->b) -> a -> [[(a,b)]]
mutationsFor f x = case errorToNothing (f x) of
                     Nothing -> []
                     Just fx -> ((,) x) `mapT` tail (mutiers fx)

-- | Returns tiers of mutants on a selection of arguments of a function.
-- Will only return the null mutant from an empty selection of arguments.
tiersMutantsOn :: (Eq a, Mutable b) => (a->b) -> [a] -> [[a->b]]
tiersMutantsOn f xs = mutate f `mapT` products (map (mutationsFor f) xs)

-- |
-- > mutants not =
-- >   [ not
-- >   , \p -> case p of False -> False; _ -> not p
-- >   , \p -> case p of True  -> True;  _ -> not p
-- >   , \p -> case p of False -> False; True -> True
-- >   ]
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  mutiers f = tiersMutantsOn f `concatMapT` setsOf tiers


-- *** *** Instances for tuples *** ***

-- | > mutants (0,1) = [(0,1),(0,0),(1,1),(0,-1),...]
instance (Mutable a, Mutable b) => Mutable (a,b) where
  mutiers (f,g) = mutiers f >< mutiers g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  mutiers (f,g,h) = productWith (\f' (g',h') -> (f',g',h'))
                                (mutiers f) (mutiers (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d)
      => Mutable (a,b,c,d) where
  mutiers (f,g,h,i) = productWith (\f' (g',h',i') -> (f',g',h',i'))
                                  (mutiers f) (mutiers (g,h,i))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e)
      => Mutable (a,b,c,d,e) where
  mutiers (f,g,h,i,j) = productWith (\f' (g',h',i',j') -> (f',g',h',i',j'))
                                    (mutiers f) (mutiers (g,h,i,j))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f)
      => Mutable (a,b,c,d,e,f) where
  mutiers (f,g,h,i,j,k) = productWith (\f' (g',h',i',j',k') ->
                                           (f',g',h',i',j',k'))
                                      (mutiers f) (mutiers (g,h,i,j,k))

-- Further tuple instances are defined on FitSpec.Mutable.Tuples and are
-- exported by default by Test.FitSpec.
