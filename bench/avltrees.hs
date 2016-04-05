{-# LANGUAGE CPP #-}
import FitSpec
import Test.Check
import AVLTree
import Data.List (sort,nubBy)

#if __GLASGOW_HASKELL__ >= 706
import Prelude hiding (insert,find)
#endif

-- TODO: separate testing of data invariants from properties over trees.

instance (Ord a, Listable a) => Listable (Tree a) where
  tiers = map (nubBy same . sort) (consFromNoDupList fromList)

instance (Ord a, Listable a) => Mutable (Tree a) where
  mutiers = mutiersEq

instance (Ord a, Show a, Listable a) => ShowMutable (Tree a) where
  mutantS = mutantSEq


-- * Tree Invariants:

ordered :: Ord a => Tree a -> Bool
ordered = ordList . flatten
  where ordList (x:y:xs) = x < y && ordList (y:xs)
        ordList _        = True

balanced :: Tree a -> Bool
balanced Empty              = True
balanced t@(Node _ lst _ rst) = abs (bf t) < 2 && balanced lst && balanced rst

underHeightLimit :: Tree a -> Bool
underHeightLimit t = n <= 2^h - 1
  where n = nElem t
        h = height t + 1

-- | Compares the height stored in the tree to an explicitly implemented version
correctHeight :: Tree a -> Bool
correctHeight t =  height t == explicitHeight t
  where
    explicitHeight Empty            = -1
    explicitHeight (Node _ lt _ gt) = max (height lt) (height gt) + 1

-- Our tiers enumeration guarantees that no mutant will produce a Tree not
-- following the invariants.  So 1-8 will always be reported as uneeded.
properties :: (Ord a, Show a, Listable a)
           => (a -> Tree a -> Tree a)
           -> (a -> Tree a -> Tree a)
           -> (a -> Tree a -> Maybe a)
           -> [Property]
properties insert remove find =
  [ property $ \x t -> ordered (insert x t)             --  1
  , property $ \x t -> ordered (remove x t)             --  2
  , property $ \x t -> balanced (insert x t)            --  3
  , property $ \x t -> balanced (remove x t)            --  4
  , property $ \x t -> underHeightLimit (insert x t)    --  5
  , property $ \x t -> underHeightLimit (remove x t)    --  6
  , property $ \x t -> correctHeight (insert x t)       --  7
  , property $ \x t -> correctHeight (remove x t)       --  8
  , property $ \x t -> find x (insert x t) == Just x    --  9
  , property $ \x t -> find x (remove x t) == Nothing   -- 10
  ]

type Insert a = a -> Tree a -> Tree a

main :: IO ()
main =
  reportWith args { names = ["insert x t","remove x t","find x t"]
                  , timeout  = 0 }
             (insert :: Insert Word2, remove, find)
             (uncurry3 properties)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z
