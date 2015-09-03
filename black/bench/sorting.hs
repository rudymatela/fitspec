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


sortI :: [Int] -> [Int]
sortI = sort

-- sorts lists of two-bit integers
sortI2 :: [UInt2] -> [UInt2]
sortI2 = sort

sortB :: [Bool] -> [Bool]
sortB = sort

sortU :: [()] -> [()]
sortU = sort

sargs :: (Listable a, Bounded a, Ord a) => Args ([a] -> [a])
sargs = args { functionName = "sort"
             , variableName = "xs"
             --, extraMutants = [sortCounter]
             , extraMutants = take 100
                            -- $ mutateBySz sort (cons1 (:) \++/ cons1 (++) \++/ cons1 (flip (++)))  -- reps
                            $ mutateBySz sort
                            $ cons1 (\x    -> (x:))
                         \++/ cons2 (\y ys -> (++ (y:ys)))
                         \++/ cons2 (\y ys -> ((y:ys) ++))
             }

csargs = cargs { functionNames = ["sort"]
               , variableNames = ["xs"]
               , nResults = Just 10
               }

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , method :: String
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 1000    &= help "number of tests to run"
  , nMutants = 1000    &= help "number of mutants to generate"
                       &= name "m"
  , testType = "int"   &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , method   = "black" &= help "method (black/grey)"
                       &= name "e"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (method as) (testType as) (nMutants as) (nTests as)

run :: String -> String -> Int -> Int -> IO ()
run "grey" "int"  = runGrey sortI
run "grey" "int2" = runGrey sortI2
run "grey" "bool" = runGrey sortB
run "grey" "unit" = runGrey sortU
run _      "int"  = runBlack sortI
run _      "int2" = runBlack sortI2
run _      "bool" = runBlack sortB
run _      "unit" = runBlack sortU
run _      _      = \_ _ -> putStrLn "unknown parameters"
runBlack f nm nt = reportWith sargs nm f (pmap nt)
runGrey f nm nt = report1With csargs nm f (pmap nt)

sortCounter :: (Bounded a, Ord a) => [a] -> [a]
sortCounter = (++ [maxBound]) . sort
