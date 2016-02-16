{-# LANGUAGE CPP #-}
import FitSpec
import Test.Check
import AVLTree
#if __GLASGOW_HASKELL__ >= 706
import Prelude hiding (insert,find)
#else
import Prelude
#endif


instance (Ord a, Listable a) => Listable (Tree a) where
  listing = cons1 fromList

instance (Ord a, Listable a) => Mutable (Tree a) where
  lsMutants = lsMutantsEq

instance (Ord a, Show a, Listable a) => ShowMutable (Tree a) where
  mutantS = mutantSEq


-- * Tree Invariants:

ordered :: Ord a => Tree a -> Bool
ordered = orderedList . flatten
  where
    orderedList []       = True
    orderedList [_]      = True
    orderedList (x:y:xs) = x < y && orderedList (y:xs)

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
    explicitHeight Empty          = -1
    explicitHeight (Node _ lt _ gt) = max (height lt) (height gt) + 1


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
  where orderedList []       = True
        orderedList [_]      = True
        orderedList (x:y:xs) = x < y && orderedList (y:xs)

sargs = args
  { nTestsF = (`div` 5)
  , minimumTime = 0
  }

main :: IO ()
main =
  reportWith sargs {nMutants = 5000}
             (insert :: Int -> Tree Int -> Tree Int, remove, find)
             (uncurry3 properties)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z
