{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
import Test.Check
import Test.Check.Utils (lsCrescListsOf)
import Mutate
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
  szMutants = lsMutantsEq

instance (Ord a, Show a, Listable a) => ShowMutable (Heap a) where
  mutantS = mutantSEq
  showMutant _ = show

-- Alias for type (they are repeated a lot)
type Insert a    = (a, Heap a) -> Heap a
type DeleteMin a = (Heap a -> Heap a)
type Merge a     = (Heap a, Heap a) -> Heap a
type Ty a        = (Insert a, DeleteMin a, Merge a)

propertyMap :: (Ord a, Show a, Listable a)
            => Int
            -> Insert a
            -> DeleteMin a
            -> Merge a
            -> [Bool]
propertyMap n insert'' deleteMin' merge'' =
  [ holds n $ \x y h ->      insert' x (insert' y h) == insert' y (insert' x h) --  1
  , holds n $ \h x ->             null (insert' x h) == False                   --  2
  , holds n $ \x h ->          L.insert x (toList h) == toList (insert' x h)    --  3

  ,                                                 True                        --  4

  , holds n $ \h h1 ->                   merge' h h1 == merge' h1 h             --  5
  , holds n $ \h ->                     merge' h Nil == h                       --  6
  , holds n $ \h h1 h2 ->    merge' h (merge' h1 h2) == merge' h1 (merge' h h2) --  7
  , holdE n $ \h -> not (null h) ==> findMin (merge' h h) == findMin h          --  8
  , holds n $ \h ->                null (merge' h h) == null h                  --  9
  , holds n $ \h h1 ->           (null h && null h1) == null (merge' h h1)      -- 10

  , holds n $ \h h1 x ->     merge' h (insert' x h1) == insert' x (merge' h h1) -- 11
  , holdE n $ \h -> not (null h) ==> merge' h (deleteMin' h) == deleteMin' (merge' h h) -- 12
  , holdE n $ \x ->       deleteMin' (insert' x Nil) == Nil                     -- 13
  ]
  where merge' = curry merge''
        insert' = curry insert''
        holdE n = errorToFalse . holds n

sargs = args { limitResults = Just 20
             , callNames = ["insert xh","deleteMin h","merge hh"]}
             -- , extraMutants = take 0 [(uncurry maxInsert,maxDeleteMin,uncurry maxMerge)] }

csargs = cargs { functionNames = ["insert","deleteMin","merge"]
               , nResults = Just 10
               }


data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , method :: String
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 500     &= help "number of tests to run"
  , nMutants = 500     &= help "number of mutants to generate"
                       &= name "m"
  , testType = "int"   &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , method   = "black" &= help "method (black/grey)"
                       &= name "e"
  }


main :: IO ()
main = do putStrLn "Heap:"
          as <- cmdArgs arguments
          run (testType as) (method as) (nTests as) (nMutants as)

fns :: Ord a => Ty a
fns = (uncurry insert, deleteMin, uncurry merge)

run :: String -> String -> Int -> Int -> IO ()
run "bool"  = run' (fns :: Ty Bool)
run "int"   = run' (fns :: Ty Int)
run "bools" = run' (fns :: Ty [Bool])

run' fs method n m =
  do unless (and $ (uncurry3 $ propertyMap n) fs) $
       putStrLn "Warning: functions being mutated *do not* follow properties"
     case method of
       "grey" -> runGrey  fs n m
       _      -> runBlack fs n m

runGrey (f,g,h) n m = report3With csargs m f g h (propertyMap n)

runBlack fs n m = reportWith sargs m fs (uncurry3 $ propertyMap n)


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
