-- | Enumeration of function mutations
module FitSpec.Mutable
  ( Mutable (..)
  , mutiersEq
--, mutantsIntegral
  )
where

import Test.Check
import Data.List (intercalate, delete)
import Data.Maybe
import Test.Check.Error (errorToNothing)

-- | This typeclass is similar to 'Listable'.
--
-- A type is 'Mutable' when there exists a function that
-- is able to list mutations of a value.
-- Ideally: list all values with no repetitions.
--
-- Instances are usually defined by a 'mutiers' function that
-- given a value, returns tiers of mutants of that value:
--   the first  tier contains the null mutant, of size 0,
--   the second tier contains mutants of size 1,
--   the third  tier contains mutants of size 2,
--   and so on.
--
-- The null mutant is the actual function without mutations.
--
-- The size of a mutant is given by the sum of:
--   the number of mutated points (relations) and
--   the sizes of mutated arguments and results.
--
-- To get only strict (non-null) mutants,
-- just take the 'tail' of either 'mutants' or 'mutiers':
--
-- > tail mutants
--
-- > tail mutiers
class Mutable a where
  mutiers :: a -> [[a]]
  mutants :: a -> [a]
  mutiers = map (:[]) . mutants
  mutants = concat . mutiers
  {-# MINIMAL mutants | mutiers #-}


-- *** *** Instances for (non-functional) data types *** ***

-- | Implementation of 'mutiers' for non-functional data types.
-- To declare:
--
-- > instance MyData where mutiers = mutiersEq
-- > instance (Eq a, Eq b) => MyDt a b where mutiers = mutiersEq
--
-- Examples:
--
-- > mutiersEq True = [[True], [False]]
-- > mutiersEq 2   = [[2], [0], [1], [], [3], [4], [5], [6], [7], [8], [9], ...
-- > mutiersEq [1] = [[[1]], [[]], [[0]], [[0,0]], [[0,0,0],[0,1],[1,0],[-1]]...
mutiersEq :: (Listable a, Eq a) => a -> [[a]]
mutiersEq x = [x] : tdelete x tiers  -- possible optimization: (tdeleteOnce x)

tdelete :: Eq a => a -> [[a]] -> [[a]]
tdelete x = tnormalize . map (delete x)
  where tnormalize []       = []
        tnormalize [[]]     = []
        tnormalize (xs:xss) = xs:tnormalize xss

-- Instances for (non-functional) data types

-- | > mutants () = [()]
instance Mutable ()   where mutiers = mutiersEq

-- | > mutants 3 = [3,0,1,2,4,5,6,7,8,9,...
instance Mutable Int  where mutiers = mutiersEq

instance Mutable Char where mutiers = mutiersEq

-- | > mutants True = [True,False]
instance Mutable Bool where mutiers = mutiersEq -- > mutants True=[True,False]

-- | > mutants [0] = [ [0], [], [0,0], [1], ...
instance (Eq a, Listable a) => Mutable [a]       where mutiers = mutiersEq

-- | > mutants (Just 0) = [Just 0, Nothing, ...
instance (Eq a, Listable a) => Mutable (Maybe a) where mutiers = mutiersEq

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
                     Just fx -> ((,) x) `tmap` tail (mutiers fx)

-- | Returns tiers of mutants on a selection of arguments of a function.
-- Will only return the null mutant from an empty selection of arguments.
tiersMutantsOn :: (Eq a, Mutable b) => (a->b) -> [a] -> [[a->b]]
tiersMutantsOn f xs = mutate f `tmap` tProducts (map (mutationsFor f) xs)

-- | Given that the underlying enumeration for argument/result values is
-- without repetitions, this instance does not repeat mutants.
--
-- > mutiers not =
-- >   [ [ not ]
-- >   , [ \p -> case p of
-- >               False -> False
-- >               _ -> not p
-- >     , \p -> case p of
-- >               True -> True
-- >               _ -> not p ]
-- >   , [ \p -> case p of
-- >               False -> False
-- >               True -> True ] ]
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  mutiers f = tiersMutantsOn f `tConcatMap` tSetsOf tiers


-- *** *** Instances for tuples *** ***

instance (Mutable a, Mutable b) => Mutable (a,b) where
  mutiers (f,g) = mutiers f `tProduct` mutiers g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  mutiers (f,g,h) = tProductWith (\f' (g',h') -> (f',g',h'))
                                 (mutiers f) (mutiers (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d)
      => Mutable (a,b,c,d) where
  mutiers (f,g,h,i) = tProductWith (\f' (g',h',i') -> (f',g',h',i'))
                                   (mutiers f) (mutiers (g,h,i))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e)
      => Mutable (a,b,c,d,e) where
  mutiers (f,g,h,i,j) = tProductWith (\f' (g',h',i',j') -> (f',g',h',i',j'))
                                     (mutiers f) (mutiers (g,h,i,j))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f)
      => Mutable (a,b,c,d,e,f) where
  mutiers (f,g,h,i,j,k) = tProductWith (\f' (g',h',i',j',k') ->
                                           (f',g',h',i',j',k'))
                                       (mutiers f) (mutiers (g,h,i,j,k))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g)
      => Mutable (a,b,c,d,e,f,g) where
  mutiers (f,g,h,i,j,k,l) = tProductWith (\f' (g',h',i',j',k',l') ->
                                             (f',g',h',i',j',k',l'))
                                         (mutiers f)
                                         (mutiers (g,h,i,j,k,l))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h)
      => Mutable (a,b,c,d,e,f,g,h) where
  mutiers (f,g,h,i,j,k,l,m) = tProductWith (\f' (g',h',i',j',k',l',m') ->
                                               (f',g',h',i',j',k',l',m'))
                                           (mutiers f)
                                           (mutiers (g,h,i,j,k,l,m))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i)
      => Mutable (a,b,c,d,e,f,g,h,i) where
  mutiers (f,g,h,i,j,k,l,m,n) =
    tProductWith (\f' (g',h',i',j',k',l',m',n') ->
                   (f',g',h',i',j',k',l',m',n'))
                 (mutiers f)
                 (mutiers (g,h,i,j,k,l,m,n))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i, Mutable j)
      => Mutable (a,b,c,d,e,f,g,h,i,j) where
  mutiers (f,g,h,i,j,k,l,m,n,o) =
    tProductWith (\f' (g',h',i',j',k',l',m',n',o') ->
                   (f',g',h',i',j',k',l',m',n',o'))
                 (mutiers f)
                 (mutiers (g,h,i,j,k,l,m,n,o))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k) where
  mutiers (f,g,h,i,j,k,l,m,n,o,p) =
    tProductWith (\f' (g',h',i',j',k',l',m',n',o',p') ->
                   (f',g',h',i',j',k',l',m',n',o',p'))
                 (mutiers f)
                 (mutiers (g,h,i,j,k,l,m,n,o,p))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k, Mutable l)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  mutiers (f,g,h,i,j,k,l,m,n,o,p,q) =
    tProductWith (\f' (g',h',i',j',k',l',m',n',o',p',q') ->
                   (f',g',h',i',j',k',l',m',n',o',p',q'))
                 (mutiers f)
                 (mutiers (g,h,i,j,k,l,m,n,o,p,q))
