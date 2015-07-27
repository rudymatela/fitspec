import FitSpec
import FitSpecC
import Test.Check
import Prelude hiding (null)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Heap
import Utils (uncurry3)

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
infixr 0 ==>

instance (Ord a, Listable a) => Listable (Heap a) where
  listing = cons1 fromList


propertyMap :: (Ord a, Show a, Listable a)
            => Int
            -> ((a,Heap a) -> Heap a)
            -> (Heap a -> Heap a)
            -> ((Heap a,Heap a) -> Heap a)
            -> [Bool]
propertyMap n insert'' deleteMin' merge'' =
  [ holds n $ \x y h ->      insert' x (insert' y h) == insert' y (insert' x h) --  1
  , holds n $ \h x ->             null (insert' x h) == False                   --  2
  , holds n $ \x h ->          L.insert x (toList h) == toList (insert' x h)    --  3

  ,                                   deleteMin' Nil == Nil                     --  4

  , holds n $ \h h1 ->                   merge' h h1 == merge' h1 h             --  5
  , holds n $ \h ->                     merge' h Nil == h                       --  6
  , holds n $ \h h1 h2 ->    merge' h (merge' h1 h2) == merge' h1 (merge' h h2) --  7
  , holds n $ \h ->             findMin (merge' h h) == findMin h               --  8
  , holds n $ \h ->                null (merge' h h) == null h                  --  9
  , holds n $ \h h1 ->           (null h && null h1) == null (merge' h h1)      -- 10

  , holds n $ \h h1 x ->     merge' h (insert' x h1) == insert' x (merge' h h1) -- 11
  , holds n $ \h ->          merge' h (deleteMin' h) == deleteMin' (merge' h h) -- 12
  , holds n $ \x ->       deleteMin' (insert' x Nil) == Nil                     -- 13
  ]
  where merge' = curry merge''
        insert' = curry insert''

csargs = cargs { functionNames = ["insert","deleteMin","merge"]
               , nResults = Just 10
               }

main :: IO ()
main = do putStrLn "Heap:"
          reportWith args { limitResults = Just 10
                          , extraMutants = take 0 [(uncurry maxInsert,maxDeleteMin,uncurry maxMerge)] }
                  -- 1000  -- 2000 --    18s -- [4,11,12] [3,4,7,12] [3,4,5,     12] [1,2,4,7,12]    [1,2,4,5,     12   ]
                  -- 6000  -- 2000 -- 1m 38s -- [4,11,12] [3,4,7,12] [3,4,5,8,   12] [1,2,4,7,12,13] [1,2,4,5,8,   12,13]
                  -- 10000 -- 2000 -- 2m 47s -- [4,11,12] [3,4,7,12] [3,4,5,8,10,12] [1,2,4,7,12,13] [1,2,4,5,8,10,12,13]
                     1000
                     (uncurry insert,(deleteMin::Heap Bool -> Heap Bool),uncurry merge)
                     (uncurry3 (propertyMap 2000))
          report3With csargs 1000 (uncurry insert)
                                  (deleteMin :: Heap Bool -> Heap Bool)
                                  (uncurry merge)
                                  (propertyMap 2000)


maxInsert :: Ord a => a -> Heap a -> Heap a
maxInsert x h = maxMerge h (branch x Nil Nil)

maxDeleteMin :: Ord a => Heap a -> Heap a
maxDeleteMin (Branch _ _ l r) = maxMerge l r
maxDeleteMin Nil              = Nil

maxMerge :: Ord a => Heap a -> Heap a -> Heap a
maxMerge Nil h = h
maxMerge h Nil = h
maxMerge h1@(Branch _ x1 l1 r1) h2@(Branch _ x2 l2 r2)
 | x1 <= x2 = branch x1 (maxMerge l1 h2) r1
 | otherwise = maxMerge h2 h1

