-- | Enumeration of function mutations
module FitSpec.Mutable
  ( Mutable (..)
  , lsMutantsEq
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
  lsMutants :: a -> [[a]]
  mutants :: a -> [a]
  lsMutants = map (:[]) . mutants
  mutants = concat . lsMutants

-- Beware: if the underlying enumeration for argument/return values generates
-- repeated elements there will be repeated and potentially null mutants.
instance (Eq a, Listable a, Mutable b) => Mutable (a -> b) where
  lsMutants f = lsmap (defaultFunPairsToFunction f)
              $ lsConcatMap (`lsAssociations'` mutantsFor)
              $ lsStrictlyAscendingListsOf listing
    where mutantsFor x = case errorToNothing (f x) of
                           Nothing -> [[]]
                           Just fx -> tail (lsMutants fx)

lsAssociations' :: [a] -> (a -> [[b]]) -> [[[(a,b)]]]
lsAssociations' xs f = lsmap (zip xs) (lsProducts (map f xs))

lsdelete :: Eq a => a -> [[a]] -> [[a]]
lsdelete x = lsnormalize . map (delete x)
  where lsnormalize []       = []
        lsnormalize [[]]     = []
        lsnormalize (xs:xss) = xs:lsnormalize xss

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
-- instance Mutable Int  where mutants = mutantsIntegral
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
  lsMutants (f,g,h) = lsProductWith (\f' (g',h') -> (f',g',h'))
                                    (lsMutants f) (lsMutants (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d)
      => Mutable (a,b,c,d) where
  lsMutants (f,g,h,i) = lsProductWith (\f' (g',h',i') -> (f',g',h',i'))
                                      (lsMutants f) (lsMutants (g,h,i))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e)
      => Mutable (a,b,c,d,e) where
  lsMutants (f,g,h,i,j) = lsProductWith (\f' (g',h',i',j') -> (f',g',h',i',j'))
                                        (lsMutants f) (lsMutants (g,h,i,j))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e, Mutable f)
      => Mutable (a,b,c,d,e,f) where
  lsMutants (f,g,h,i,j,k) = lsProductWith (\f' (g',h',i',j',k') ->
                                            (f',g',h',i',j',k'))
                                          (lsMutants f) (lsMutants (g,h,i,j,k))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g)
      => Mutable (a,b,c,d,e,f,g) where
  lsMutants (f,g,h,i,j,k,l) = lsProductWith (\f' (g',h',i',j',k',l') ->
                                              (f',g',h',i',j',k',l'))
                                            (lsMutants f)
                                            (lsMutants (g,h,i,j,k,l))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h)
      => Mutable (a,b,c,d,e,f,g,h) where
  lsMutants (f,g,h,i,j,k,l,m) = lsProductWith (\f' (g',h',i',j',k',l',m') ->
                                                (f',g',h',i',j',k',l',m'))
                                              (lsMutants f)
                                              (lsMutants (g,h,i,j,k,l,m))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i)
      => Mutable (a,b,c,d,e,f,g,h,i) where
  lsMutants (f,g,h,i,j,k,l,m,n) =
    lsProductWith (\f' (g',h',i',j',k',l',m',n') ->
                    (f',g',h',i',j',k',l',m',n'))
                  (lsMutants f)
                  (lsMutants (g,h,i,j,k,l,m,n))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i, Mutable j)
      => Mutable (a,b,c,d,e,f,g,h,i,j) where
  lsMutants (f,g,h,i,j,k,l,m,n,o) =
    lsProductWith (\f' (g',h',i',j',k',l',m',n',o') ->
                    (f',g',h',i',j',k',l',m',n',o'))
                  (lsMutants f)
                  (lsMutants (g,h,i,j,k,l,m,n,o))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k) where
  lsMutants (f,g,h,i,j,k,l,m,n,o,p) =
    lsProductWith (\f' (g',h',i',j',k',l',m',n',o',p') ->
                    (f',g',h',i',j',k',l',m',n',o',p'))
                  (lsMutants f)
                  (lsMutants (g,h,i,j,k,l,m,n,o,p))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k, Mutable l)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  lsMutants (f,g,h,i,j,k,l,m,n,o,p,q) =
    lsProductWith (\f' (g',h',i',j',k',l',m',n',o',p',q') ->
                    (f',g',h',i',j',k',l',m',n',o',p',q'))
                  (lsMutants f)
                  (lsMutants (g,h,i,j,k,l,m,n,o,p,q))
