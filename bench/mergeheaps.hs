{-# Language DeriveDataTypeable, NoMonomorphismRestriction #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import Prelude hiding (null)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Heap
import Control.Monad (unless)

instance (Ord a, Listable a) => Listable (Heap a) where
  tiers = consFromAscendingList fromList

instance (Ord a, Listable a) => Mutable (Heap a) where
  mutiers = mutiersEq 

instance (Ord a, Show a, Listable a) => ShowMutable (Heap a) where
  mutantS = mutantSEq

type Merge a = Heap a -> Heap a -> Heap a
type Ty a = Merge a

properties :: (Ord a, Show a, Listable a)
           => Merge a
           -> [Property]
properties merge =
  [ property $ \h h1    -> merge h h1 == merge h1 h
  , property $ \h       -> merge h Nil == h
  , property $ \x h h1  -> merge h (insert x h1) == insert x (merge h h1)
  , property $ \h h1 h2 -> merge h (merge h1 h2) == merge h1 (merge h h2)
  , property $ \h       -> notNull h ==> findMin (merge h h) == findMin h
  , property $ \h       -> null (merge h h) == null h
  , property $ \h       -> notNull h ==> merge h (deleteMin h) == deleteMin (merge h h)
  , property $ \h h1    -> (null h && null h1) == null (merge h h1)
--, property $ \xs ys   -> merge (fromList xs) (fromList ys) == fromList (xs++ys)
--, property $ \h h1    -> mergeLists (toList h) (toList h1) == toList (merge h h1)
  ]
  where notNull = not . null

sargs = args
  { timeout  = 0
  , nMutants = 500
  , nTests   = 500
  , names    = ["merge h h'"]
  }

em :: (Bounded a, Ord a) => [Ty a]
em = take 4
  [ (\_ _ -> Nil)
  , maxMerge
  , crazyMerge
  , mergeEqNil
  ]

main :: IO ()
main = do 
  as <- getArgsWith sargs
  let run f = reportWithExtra em as f properties
  case (extra as) of
--  "bool"  -> run (merge :: Ty Bool)
--  "bools" -> run (merge :: Ty [Bool])
    "i"     -> run (merge :: Ty Int)
    "i1"    -> run (merge :: Ty Int1)
    "i2"    -> run (merge :: Ty Int2)
    "i3"    -> run (merge :: Ty Int3)
    "w1"    -> run (merge :: Ty Word1)
    "w2"    -> run (merge :: Ty Word2)
    "w3"    -> run (merge :: Ty Word3)
    "unit"  -> run (merge :: Ty ())
    ""      -> run (merge :: Ty Word2)


maxInsert :: Ord a => a -> Heap a -> Heap a
maxInsert x h = maxMerge h (branch x Nil Nil)

maxDeleteMin :: Ord a => Heap a -> Heap a
maxDeleteMin (Branch _ _ l r) = maxMerge l r
maxDeleteMin Nil              = Nil

maxMerge :: Ord a => Heap a -> Heap a -> Heap a
maxMerge Nil h = h
maxMerge h Nil = h
maxMerge h1@(Branch _ x1 l1 r1) h2@(Branch _ x2 l2 r2)
 | x1 >= x2 = branch x1 (maxMerge l1 h2) r1
 | otherwise = maxMerge h2 h1

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

crazyMerge :: (Bounded a, Ord a) => Heap a -> Heap a -> Heap a
crazyMerge Nil Nil = Nil
crazyMerge Nil h = h
crazyMerge h Nil = h
crazyMerge h h1 = insert maxBound $ merge h h1

mergeEqNil :: (Ord a) => Heap a -> Heap a -> Heap a
mergeEqNil h h1 | h == h1   = Nil
                | otherwise = merge h h1
