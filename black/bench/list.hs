{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
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
      => Bool -> Int -> Int
      -> Args (Ty a)
sargs em nt nm = args
  { callNames = ["(:) x xs","head xs","tail xs","(++) xs ys"]
  , limitResults = Just 30
  , extraMutants = takeWhile (const em)
                 [ ((:),head,tail,(++-))
                 , ((:),head,tail,(++--))
                 ]
  , nMutants = nm
  , nTestsF = const nt
  , minimumTime = 0
  }

csargs = cargs { functionNames = [":","head","tail","++"]
               , variableNames = ["p","xs","xs","p"]
               , nResults = Just 30
               }

data CmdArguments = CmdArguments
  { nMutants_ :: Int
  , nTests :: Int
  , testType :: String
  , classify :: Bool
  , useExtraMutants :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests    = 1000   &= help "number of tests to run"
  , nMutants_ = 1000   &= help "number of mutants to generate"
                       &= name "m"
  , testType = "bool"  &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , classify = False   &= help "classify mutants, report extra column with fully evaluated ones (grey-box)"
  , useExtraMutants = False
                       &= help "pass extra manual mutants to the algorithm (only works for black-box version)"
                       &= name "e"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (classify as) (useExtraMutants as) (nMutants_ as) (nTests as)

run :: String -> Bool -> Bool -> Int -> Int -> IO ()
run "int"   = run' (fns :: Ty Int)
run "int2"  = run' (fns :: Ty UInt2)
run "int3"  = run' (fns :: Ty UInt3)
run "bool"  = run' (fns :: Ty Bool)
run "bools" = run' (fns :: Ty [Bool])
run "unit"  = run' (fns :: Ty ())
run' f False em nm nt = reportWith (sargs em nt nm) f (uncurry4 . pmap)
-- run' f True  em nm nt = report1With csargs nm f (pmap nt) -- TODO


-- Some manual mutants
(++-) :: [a] -> [a] -> [a]
xs ++- ys = []

(++--) :: [a] -> [a] -> [a]
xs ++-- ys = if length xs > length ys
               then xs
               else ys
