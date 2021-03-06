-- Heap code from QuickSpec examples.
-- https://github.com/nick8325/quickspec/blob/0.9.6/examples/Heaps.hs
--
-- Copyright (c) 2009-2014, Nick Smallbone
-- https://github.com/nick8325/quickspec/blob/0.9.6/LICENSE (BSD3 license)
module Heap where

data Heap a = Nil | Branch Int a (Heap a) (Heap a) deriving Show

instance Ord a => Eq (Heap a) where
  h1 == h2 = toList h1 == toList h2

instance Ord a => Ord (Heap a) where
  h1 `compare` h2 = toList h1 `compare` toList h2

toList :: Ord a => Heap a -> [a]
toList Nil = []
toList h   = findMin h : toList (deleteMin h)

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert Nil

null :: Heap a -> Bool
null Nil = True
null _ = False

findMin :: Heap a -> a
findMin (Branch _ x _ _) = x

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge h (branch x Nil Nil)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Branch _ _ l r) = merge l r

branch :: Ord a => a -> Heap a -> Heap a -> Heap a
branch x l r | npl l <= npl r = Branch (npl l + 1) x l r
             | otherwise = Branch (npl r + 1) x r l

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge h1@(Branch _ x1 l1 r1) h2@(Branch _ x2 l2 r2)
 | x1 <= x2 = branch x1 (merge l1 h2) r1
 | otherwise = merge h2 h1

npl :: Heap a -> Int
npl Nil = 0
npl (Branch n _ _ _) = n
