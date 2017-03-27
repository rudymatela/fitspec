-- |
-- Module      : Test.FitSpec.Tuples
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- ShowMutable instances: septuples up to 12-tuples
--
-- This is partly a Hack that allows those instances to be hidden from Haddock.
-- Otherwise Haddock documentation will look very ugly.
-- It also makes "Test.FitSpec.ShowMutable" more readable.
module Test.FitSpec.ShowMutable.Tuples () where

import Test.FitSpec.ShowMutable

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g)
      => ShowMutable (a,b,c,d,e,f,g) where
  mutantS (f,g,h,i,j,k,l) (f',g',h',i',j',k',l') = mutantSTuple
                                                    [ mutantS f f'
                                                    , mutantS g g'
                                                    , mutantS h h'
                                                    , mutantS i i'
                                                    , mutantS j j'
                                                    , mutantS k k'
                                                    , mutantS l l' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h)
      => ShowMutable (a,b,c,d,e,f,g,h) where
  mutantS (f,g,h,i,j,k,l,m) (f',g',h',i',j',k',l',m') = mutantSTuple
                                                      [ mutantS f f'
                                                      , mutantS g g'
                                                      , mutantS h h'
                                                      , mutantS i i'
                                                      , mutantS j j'
                                                      , mutantS k k'
                                                      , mutantS l l'
                                                      , mutantS m m' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i)
      => ShowMutable (a,b,c,d,e,f,g,h,i) where
  mutantS (f,g,h,i,j,k,l,m,n) (f',g',h',i',j',k',l',m',n') = mutantSTuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j)
      => ShowMutable (a,b,c,d,e,f,h,g,i,j) where
  mutantS (f,g,h,i,j,k,l,m,n,o) (f',g',h',i',j',k',l',m',n',o') = mutantSTuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n'
    , mutantS o o' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j, ShowMutable k)
      => ShowMutable (a,b,c,d,e,f,g,h,i,j,k) where
  mutantS (f,g,h,i,j,k,l,m,n,o,p) (f',g',h',i',j',k',l',m',n',o',p') = mutantSTuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n'
    , mutantS o o'
    , mutantS p p' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j, ShowMutable k, ShowMutable l)
      => ShowMutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  mutantS (f,g,h,i,j,k,l,m,n,o,p,q) (f',g',h',i',j',k',l',m',n',o',p',q') = mutantSTuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n'
    , mutantS o o'
    , mutantS p p'
    , mutantS q q' ]
