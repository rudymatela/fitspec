{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec.Main
import Data.List
import Test.Check
import Test.Types
import Test.Types.Mutate
import Utils (errorToFalse, uncurry4)

type Cons a = a -> [a] -> [a]
type Head a = [a] -> a
type Tail a = [a] -> [a]
type Append a = [a] -> [a] -> [a]
type Ty a = ( Cons a
            , Head a
            , Tail a
            , Append a
            )

-- The property map
pmap :: (Eq a, Show a, Listable a)
     => Int
     -> Cons a
     -> Head a
     -> Tail a
     -> Append a
     -> [Bool]
pmap n (-:) head tail (++) =
  [ holds n $ \xs -> [] ++ xs == xs && xs == xs ++ []
  , holdE n $ \x xs -> head (x-:xs) == x  -- mutated (:) might return an empty list
  , holdE n $ \x xs -> tail (x-:xs) == xs -- mutated (:) might return an empty list
  , holds n $ \xs -> null (xs ++ xs) == null xs
  , holds n $ \xs ys zs -> (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
  , holds n $ \x xs ys -> x-:(xs ++ ys) == (x-:xs) ++ ys
  ]
  where holdE n = errorToFalse . holds n


fns :: Ty a
fns = ((:),head,tail,(++))

sargs :: (ShowMutable a, Eq a, Show a, Listable a)
      => Args (Ty a)
sargs = args
  { callNames = ["(:) x xs","head xs","tail xs","(++) xs ys"]
  , limitResults = Just 30
  , extraMutants = takeWhile (const False)
                 [ ((:),head,tail,(++-))
                 , ((:),head,tail,(++--))
                 ]
  , nMutants = 1000
  , nTestsF  = id
  , minimumTime = 0
  }

main :: IO ()
main = do 
  let run f = mainWith sargs f (uncurry4 . pmap)
  ty <- typeArgument
  case ty of
    "bool"  -> run (fns :: Ty Bool)
    "bools" -> run (fns :: Ty [Bool])
    "int"   -> run (fns :: Ty Int)
    "int2"  -> run (fns :: Ty UInt2)
    "int3"  -> run (fns :: Ty UInt3)
    "unit"  -> run (fns :: Ty ())
    _       -> run (fns :: Ty UInt2)

-- Some manual mutants
(++-) :: [a] -> [a] -> [a]
xs ++- ys = []

(++--) :: [a] -> [a] -> [a]
xs ++-- ys = if length xs > length ys
               then xs
               else ys
