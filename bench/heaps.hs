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

-- a good property to assure that the above does not leave out elements is:
--
-- \xs ys =  xs `permutation` ys  <==>  fromList xs == fromList ys
--                                          `asTypeOf` (undefined :: Heap a)

instance (Ord a, Listable a) => Mutable (Heap a) where
  mutiers = mutiersEq 

instance (Ord a, Show a, Listable a) => ShowMutable (Heap a) where
  mutantS = mutantSEq

-- Alias for type (they are repeated a lot)
type Insert a    = a -> Heap a -> Heap a
type DeleteMin a = Heap a -> Heap a
type Merge a     = Heap a -> Heap a -> Heap a
type Ty a        = (Insert a, DeleteMin a, Merge a)

properties :: (Ord a, Show a, Listable a)
           => Insert a
           -> DeleteMin a
           -> Merge a
           -> [Property]
properties insert' deleteMin' merge' =
  [ property  $ \x y h ->      insert' x (insert' y h) == insert' y (insert' x h) --  1
  , property  $ \h x ->             null (insert' x h) == False                   --  2
  , property  $ \x h ->          L.insert x (toList h) == toList (insert' x h)    --  3

  , property  $ \h h1 ->                   merge' h h1 == merge' h1 h             --  4
  , property  $ \h ->                     merge' h Nil == h                       --  5
  , property  $ \h h1 h2 ->    merge' h (merge' h1 h2) == merge' h1 (merge' h h2) --  6
  , propertyE $ \h -> not (null h) ==> findMin (merge' h h) == findMin h          --  7
  , property  $ \h ->                null (merge' h h) == null h                  --  8
  , property  $ \h h1 ->           (null h && null h1) == null (merge' h h1)      --  9

  , property  $ \h h1 x ->     merge' h (insert' x h1) == insert' x (merge' h h1) -- 10
  , propertyE $ \h -> not (null h) ==> merge' h (deleteMin' h) == deleteMin' (merge' h h) -- 11
  , propertyE $ \x ->       deleteMin' (insert' x Nil) == Nil                     -- 12
  ]

sargs = args
  { timeout  = 0
  , nMutants = 500
  , nTests   = 500
  , names    = ["insert x h","deleteMin h","merge h h'"]
--, extraMutants = take 0 [(uncurry maxInsert,maxDeleteMin,uncurry maxMerge)] }
  }

fns :: Ord a => Ty a
fns = (insert, deleteMin, merge)

em :: Ord a => [Ty a]
em = take 0
  [ (\i h -> Nil, deleteMin, merge) ]

main :: IO ()
main = do 
  as <- getArgsWith sargs
  let run f = reportWithExtra em as f (uncurry3 properties)
  case (extra as) of
    "bool"  -> run (fns :: Ty Bool)
    "bools" -> run (fns :: Ty [Bool])
    "i"     -> run (fns :: Ty Int)
    "i1"    -> run (fns :: Ty Int1)
    "i2"    -> run (fns :: Ty Int2)
    "i3"    -> run (fns :: Ty Int3)
    "w1"    -> run (fns :: Ty Word1)
    "w2"    -> run (fns :: Ty Word2)
    "w3"    -> run (fns :: Ty Word3)
    "unit"  -> run (fns :: Ty ())
    ""      -> run (fns :: Ty Word2)

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z
