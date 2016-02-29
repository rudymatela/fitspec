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

-- The first mutant returned by szMutants and mutants is the actual function
-- without mutation.
class Mutable a where
  tMutants :: a -> [[a]]
  mutants :: a -> [a]
  tMutants = map (:[]) . mutants
  mutants = concat . tMutants

-- Beware: if the underlying enumeration for argument/return values generates
-- repeated elements there will be repeated and potentially null mutants.
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  tMutants f = tmap (defaultFunPairsToFunction f)
             $ tConcatMap (`tAssociations'` mutantsFor)
             $ tStrictlyAscendingListsOf tiers
    where mutantsFor x = case errorToNothing (f x) of
                           Nothing -> [[]]
                           Just fx -> tail (tMutants fx)

tAssociations' :: [a] -> (a -> [[b]]) -> [[[(a,b)]]]
tAssociations' xs f = tmap (zip xs) (tProducts (map f xs))

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
