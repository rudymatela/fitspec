{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
import Data.List
import Test.Check
import Test.Types


ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

permutation :: Eq a => [a] -> [a] -> Bool
[]     `permutation` []    = True
(_:_)  `permutation` []    = False
[]     `permutation` (_:_) = False
(x:xs) `permutation` ys    = x `elem` ys  &&  xs `permutation` delete x ys

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)


-- The property map
pmap :: (Ord a, Show a, Listable a) => Int -> ([a] -> [a]) -> [Bool]
pmap n sort' =
  [ holds n $ \xs ->          ordered (sort' xs)
  , holds n $ \xs ->           length (sort' xs) == length xs
  , holds n $ \x xs ->         elem x (sort' xs) == elem x xs
  , holds n $ \x xs ->        count x (sort' xs) == count x xs
  , holds n $ \xs ->   permutation xs (sort' xs)
  , holds n $ \xs ->            sort' (sort' xs) == sort' xs
  , holds n $ \x xs ->       insert x (sort' xs) == sort' (x:xs)
  ]


sargs :: (Listable a, Bounded a, Ord a) => Bool -> Args ([a] -> [a])
sargs em = args
             { callNames = ["sort xs"]
             --, extraMutants = [sortCounter]
             , extraMutants = take (if em then 100 else 0)
                            $ concat
                            $ lsmap (. sort)
                            $ cons2 (\y ys -> (++ (y:ys))) -- prepend non-empty list
                         \++/ cons2 (\y ys -> ((y:ys) ++)) -- append non-empty list
             }

csargs = cargs { functionNames = ["sort"]
               , variableNames = ["xs"]
               , nResults = Just 10
               }

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , classify :: Bool
  , useExtraMutants :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 1000    &= help "number of tests to run"
  , nMutants = 1000    &= help "number of mutants to generate"
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
          run (testType as) (classify as) (useExtraMutants as) (nMutants as) (nTests as)

type Ty a = [a] -> [a]

run :: String -> Bool -> Bool -> Int -> Int -> IO ()
run "bool"  = run' (sort :: Ty Bool)
run "bools" = run' (sort :: Ty [Bool])
run "int"   = run' (sort :: Ty Int)
run "int2"  = run' (sort :: Ty UInt2)
run "int3"  = run' (sort :: Ty UInt3)
run "unit"  = run' (sort :: Ty ())
run' f False em nm nt = reportWith (sargs em) nm f (pmap nt)
run' f True  em nm nt = report1With csargs nm f (pmap nt)

-- This hack bounded instace is only necessary when using sortCounter as a
-- manual mutant when sorting lists of things
instance Bounded a => Bounded [a] where
  minBound = []
  maxBound = repeat maxBound -- non terminating upper bound

sortCounter :: (Bounded a, Ord a) => [a] -> [a]
sortCounter = (++ [maxBound]) . sort
