{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import Data.List
import Test.Check

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
properties :: (Eq a, Show a, Listable a)
           => Cons a
           -> Head a
           -> Tail a
           -> Append a
           -> [Property]
properties (-:) head tail (++) =
  [ property  $ \xs -> [] ++ xs == xs && xs == xs ++ []
  , propertyE $ \x xs -> head (x-:xs) == x  -- mutated (:) might return an empty list
  , propertyE $ \x xs -> tail (x-:xs) == xs -- mutated (:) might return an empty list
  , property  $ \xs -> null (xs ++ xs) == null xs
  , property  $ \xs ys zs -> (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
  , property  $ \x xs ys -> x-:(xs ++ ys) == (x-:xs) ++ ys
  ]


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
  let run f = mainWith sargs f (uncurry4 properties)
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

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (x,y,z,w) = f x y z w

