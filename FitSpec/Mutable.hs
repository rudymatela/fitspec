-- | Enumeration of function mutations
module FitSpec.Mutable
  ( Mutable (..)
  , tMutantsEq
  , mutantsIntegral
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
class Mutable a where
  tMutants :: a -> [[a]]
  mutants :: a -> [a]
  tMutants = map (:[]) . mutants
  mutants = concat . tMutants
  {-# MINIMAL mutants | tMutants #-}

-- Beware: if the underlying enumeration for argument/return values generates
-- repeated elements there will be repeated and potentially null mutants.
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  tMutants f = tiersMutantsOn f `tConcatMap` tSetsOf tiers

-- | Returns tiers of mutants on a selection of arguments.
tiersMutantsOn :: (Eq a, Mutable b) => (a->b) -> [a] -> [[a->b]]
tiersMutantsOn f xs = mutate f `tmap` tProducts (map (mutationsFor f) xs)

-- | Return tiers of possible mutations for a single value.
-- If the function is undefined at that point,
-- no mutations are provided.
mutationsFor :: Mutable b => (a->b) -> a -> [[(a,b)]]
mutationsFor f x = case errorToNothing (f x) of
                     Nothing -> []
                     Just fx -> ((,) x) `tmap` tail (tMutants fx)

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

tdelete :: Eq a => a -> [[a]] -> [[a]]
tdelete x = tnormalize . map (delete x)
  where tnormalize []       = []
        tnormalize [[]]     = []
        tnormalize (xs:xss) = xs:tnormalize xss

-- TODO: Possible Optimization: (deleteOnce x)
tMutantsEq :: (Listable a, Eq a) => a -> [[a]]
tMutantsEq x = [x] : tdelete x tiers

-- Mutants of an Integral value.  Always start towards zero.  Alternating
-- between sucessor and predecessor.
-- The tail usage is there to avoid generating out of bound values.
-- (x-1) is usually safe though.
mutantsIntegral :: Integral a => a -> [a]
mutantsIntegral i | i > 0     = [i..] +| tail [i,(i-1)..]
                  | otherwise = [i,(i-1)..] +| tail [i..]

instance Mutable ()   where tMutants = tMutantsEq
-- instance Mutable Int  where mutants = mutantsIntegral
instance Mutable Int  where tMutants = tMutantsEq
instance Mutable Char where tMutants = tMutantsEq
instance Mutable Bool where tMutants = tMutantsEq
instance (Eq a, Listable a) => Mutable [a]       where tMutants = tMutantsEq
instance (Eq a, Listable a) => Mutable (Maybe a) where tMutants = tMutantsEq

{- Alternative implementation for Mutable lists:
instance (Listable a, Mutable a) => Mutable [a]
  where tMutants []     = [ [] ]
                         : [ ]
                         : tail listing
        tMutants (x:xs) = [ (x:xs) ]
                         : [ [] ]
                         : tail (lsProductWith (:) (tMutants x) (tMutants xs))
-- -}

instance (Mutable a, Mutable b) => Mutable (a,b) where
  tMutants (f,g) = tMutants f `tProduct` tMutants g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  tMutants (f,g,h) = tProductWith (\f' (g',h') -> (f',g',h'))
                                  (tMutants f) (tMutants (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d)
      => Mutable (a,b,c,d) where
  tMutants (f,g,h,i) = tProductWith (\f' (g',h',i') -> (f',g',h',i'))
                                    (tMutants f) (tMutants (g,h,i))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e)
      => Mutable (a,b,c,d,e) where
  tMutants (f,g,h,i,j) = tProductWith (\f' (g',h',i',j') -> (f',g',h',i',j'))
                                      (tMutants f) (tMutants (g,h,i,j))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f)
      => Mutable (a,b,c,d,e,f) where
  tMutants (f,g,h,i,j,k) = tProductWith (\f' (g',h',i',j',k') ->
                                           (f',g',h',i',j',k'))
                                        (tMutants f) (tMutants (g,h,i,j,k))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g)
      => Mutable (a,b,c,d,e,f,g) where
  tMutants (f,g,h,i,j,k,l) = tProductWith (\f' (g',h',i',j',k',l') ->
                                             (f',g',h',i',j',k',l'))
                                          (tMutants f)
                                          (tMutants (g,h,i,j,k,l))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h)
      => Mutable (a,b,c,d,e,f,g,h) where
  tMutants (f,g,h,i,j,k,l,m) = tProductWith (\f' (g',h',i',j',k',l',m') ->
                                               (f',g',h',i',j',k',l',m'))
                                            (tMutants f)
                                            (tMutants (g,h,i,j,k,l,m))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i)
      => Mutable (a,b,c,d,e,f,g,h,i) where
  tMutants (f,g,h,i,j,k,l,m,n) =
    tProductWith (\f' (g',h',i',j',k',l',m',n') ->
                   (f',g',h',i',j',k',l',m',n'))
                 (tMutants f)
                 (tMutants (g,h,i,j,k,l,m,n))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i, Mutable j)
      => Mutable (a,b,c,d,e,f,g,h,i,j) where
  tMutants (f,g,h,i,j,k,l,m,n,o) =
    tProductWith (\f' (g',h',i',j',k',l',m',n',o') ->
                   (f',g',h',i',j',k',l',m',n',o'))
                 (tMutants f)
                 (tMutants (g,h,i,j,k,l,m,n,o))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k) where
  tMutants (f,g,h,i,j,k,l,m,n,o,p) =
    tProductWith (\f' (g',h',i',j',k',l',m',n',o',p') ->
                   (f',g',h',i',j',k',l',m',n',o',p'))
                 (tMutants f)
                 (tMutants (g,h,i,j,k,l,m,n,o,p))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k, Mutable l)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  tMutants (f,g,h,i,j,k,l,m,n,o,p,q) =
    tProductWith (\f' (g',h',i',j',k',l',m',n',o',p',q') ->
                   (f',g',h',i',j',k',l',m',n',o',p',q'))
                 (tMutants f)
                 (tMutants (g,h,i,j,k,l,m,n,o,p,q))
