import FitSpec
import FitSpecC
import Mutate.Show
import Test.Check
import Mutate
import AVLTree
import Utils (errorToFalse)
import Prelude hiding (insert,find)


instance (Ord a, Listable a) => Listable (Tree a) where
  listing = cons1 fromList

instance (Ord a, Listable a) => Mutable (Tree a) where
  lsMutants = lsMutantsEq

instance (Ord a, Show a, Listable a) => ShowMutable (Tree a) where
  mutantS = mutantSEq
  showMutant _ = show


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


propertyMap :: (Ord a, Show a, Listable a)
            => Int
            -> ((a,Tree a) -> Tree a)
            -> ((a,Tree a) -> Tree a)
            -> ((a,Tree a) -> Maybe a)
            -> [Bool]
propertyMap n insert'' remove'' find'' =
  [ holds n $ \x t -> ordered (insert' x t)             --  1
  , holds n $ \x t -> ordered (remove' x t)             --  2
  , holds n $ \x t -> balanced (insert' x t)            --  3
  , holds n $ \x t -> balanced (remove' x t)            --  4
  , holds n $ \x t -> underHeightLimit (insert' x t)    --  5
  , holds n $ \x t -> underHeightLimit (remove' x t)    --  6
  , holds n $ \x t -> correctHeight (insert' x t)       --  7
  , holds n $ \x t -> correctHeight (remove' x t)       --  8
  , holds n $ \x t -> find' x (insert' x t) == Just x   --  9
  , holds n $ \x t -> find' x (remove' x t) == Nothing  -- 10
  ]
  where insert' = curry insert''
        remove' = curry remove''
        find'   = curry find''
        holdE n = errorToFalse . holds n
        orderedList []       = True
        orderedList [_]      = True
        orderedList (x:y:xs) = x < y && orderedList (y:xs)


csargs = cargs { functionNames = ["insert","remove","find"]
               , nResults = Just 10
               }

sargs = args { nTestsF = (`div` 5) }

main :: IO ()
main = do reportWith sargs 5000 (uncurry insert :: (Int,Tree Int) -> Tree Int, uncurry remove, uncurry find) pmap
          report3 5000 (uncurry insert :: (Int,Tree Int) -> Tree Int) (uncurry remove) (uncurry find) (propertyMap 1000)
  where pmap = uncurry3 . propertyMap


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z
