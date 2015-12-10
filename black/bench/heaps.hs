{-# Language DeriveDataTypeable, NoMonomorphismRestriction #-}
import System.Console.CmdArgs hiding (args)
import FitSpec.Main
import Test.Check
import Test.Check.Utils (lsCrescListsOf)
import Test.Types
import Test.Types.Mutate
import Mutate
import Mutate.Show
import Prelude hiding (null)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Heap
import Utils (uncurry3,errorToFalse)
import Control.Monad (unless)

instance (Ord a, Listable a) => Listable (Heap a) where
  listing = lsmap fromList
          $ lsCrescListsOf listing -- :: [a]

-- a good property to assure that the above does not leave out elements is:
--
-- \xs ys =  xs `permutation` ys  <==>  fromList xs == fromList ys
--                                          `asTypeOf` (undefined :: Heap a)

instance (Ord a, Listable a) => Mutable (Heap a) where
  lsMutants = lsMutantsEq

instance (Ord a, Show a, Listable a) => ShowMutable (Heap a) where
  mutantS = mutantSEq
  showMutant _ = show

-- Alias for type (they are repeated a lot)
type Insert a    = a -> Heap a -> Heap a
type DeleteMin a = Heap a -> Heap a
type Merge a     = Heap a -> Heap a -> Heap a
type Ty a        = (Insert a, DeleteMin a, Merge a)

propertyMap :: (Ord a, Show a, Listable a)
            => Int
            -> Insert a
            -> DeleteMin a
            -> Merge a
            -> [Bool]
propertyMap n insert' deleteMin' merge' =
  [ holds n $ \x y h ->      insert' x (insert' y h) == insert' y (insert' x h) --  1
  , holds n $ \h x ->             null (insert' x h) == False                   --  2
  , holds n $ \x h ->          L.insert x (toList h) == toList (insert' x h)    --  3

  , holds n $ \h h1 ->                   merge' h h1 == merge' h1 h             --  4
  , holds n $ \h ->                     merge' h Nil == h                       --  5
  , holds n $ \h h1 h2 ->    merge' h (merge' h1 h2) == merge' h1 (merge' h h2) --  6
  , holdE n $ \h -> not (null h) ==> findMin (merge' h h) == findMin h          --  7
  , holds n $ \h ->                null (merge' h h) == null h                  --  8
  , holds n $ \h h1 ->           (null h && null h1) == null (merge' h h1)      --  9

  , holds n $ \h h1 x ->     merge' h (insert' x h1) == insert' x (merge' h h1) -- 10
  , holdE n $ \h -> not (null h) ==> merge' h (deleteMin' h) == deleteMin' (merge' h h) -- 11
  , holdE n $ \x ->       deleteMin' (insert' x Nil) == Nil                     -- 12
  ]
  where holdE n = errorToFalse . holds n

sargs = args
  { limitResults = Just 20
  , nMutants = 500
  , nTestsF  = id
  , minimumTime = 0
  , showPropertySets = unlines
  , callNames = ["insert x h","deleteMin h","merge h h'"]
--, extraMutants = take 0 [(uncurry maxInsert,maxDeleteMin,uncurry maxMerge)] }
  }

fns :: Ord a => Ty a
fns = (insert, deleteMin, merge)

main :: IO ()
main = do 
  let run f = mainWith sargs f (uncurry3 . propertyMap)
  ty <- typeArgument
  case ty of
    "bool"  -> run (fns :: Ty Bool)
    "bools" -> run (fns :: Ty [Bool])
    "int"   -> run (fns :: Ty Int)
    "int2"  -> run (fns :: Ty UInt2)
    "int3"  -> run (fns :: Ty UInt3)
    "unit"  -> run (fns :: Ty ())
    _       -> run (fns :: Ty UInt2)


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
