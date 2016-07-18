-- | Mutable instances: septuples up to 12-tuples
--
-- This is part of a Hack that allows those instances to be hidden from Haddock.
-- Otherwise Haddock documentation will look very ugly.
module Test.FitSpec.Mutable.Tuples () where

import Test.FitSpec.Mutable
import Test.LeanCheck (productWith)

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g)
      => Mutable (a,b,c,d,e,f,g) where
  mutiers (f,g,h,i,j,k,l) = productWith (\f' (g',h',i',j',k',l') ->
                                             (f',g',h',i',j',k',l'))
                                        (mutiers f)
                                        (mutiers (g,h,i,j,k,l))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h)
      => Mutable (a,b,c,d,e,f,g,h) where
  mutiers (f,g,h,i,j,k,l,m) = productWith (\f' (g',h',i',j',k',l',m') ->
                                               (f',g',h',i',j',k',l',m'))
                                          (mutiers f)
                                          (mutiers (g,h,i,j,k,l,m))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i)
      => Mutable (a,b,c,d,e,f,g,h,i) where
  mutiers (f,g,h,i,j,k,l,m,n) =
    productWith (\f' (g',h',i',j',k',l',m',n') ->
                   (f',g',h',i',j',k',l',m',n'))
                (mutiers f)
                (mutiers (g,h,i,j,k,l,m,n))

instance (Mutable a, Mutable b, Mutable c, Mutable d, Mutable e,
          Mutable f, Mutable g, Mutable h, Mutable i, Mutable j)
      => Mutable (a,b,c,d,e,f,g,h,i,j) where
  mutiers (f,g,h,i,j,k,l,m,n,o) =
    productWith (\f' (g',h',i',j',k',l',m',n',o') ->
                   (f',g',h',i',j',k',l',m',n',o'))
                (mutiers f)
                (mutiers (g,h,i,j,k,l,m,n,o))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k) where
  mutiers (f,g,h,i,j,k,l,m,n,o,p) =
    productWith (\f' (g',h',i',j',k',l',m',n',o',p') ->
                   (f',g',h',i',j',k',l',m',n',o',p'))
                (mutiers f)
                (mutiers (g,h,i,j,k,l,m,n,o,p))

instance (Mutable a, Mutable b, Mutable c, Mutable d,
          Mutable e, Mutable f, Mutable g, Mutable h,
          Mutable i, Mutable j, Mutable k, Mutable l)
      => Mutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  mutiers (f,g,h,i,j,k,l,m,n,o,p,q) =
    productWith (\f' (g',h',i',j',k',l',m',n',o',p',q') ->
                   (f',g',h',i',j',k',l',m',n',o',p',q'))
                (mutiers f)
                (mutiers (g,h,i,j,k,l,m,n,o,p,q))
