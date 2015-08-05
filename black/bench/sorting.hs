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


mapProps :: (Ord a, Show a, Listable a) => ([a] -> [a]) -> Int -> [Bool]
mapProps sort' n =
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

main :: IO ()
main = do putStrLn "### Strict mutant enumerations ###"

          putStrLn "Booleans:"
          reportWith sargs  1000 sortB (`mapProps` 1000)
          putStrLn "Integers:"
          reportWith sargs  1000 sortI (`mapProps` 1000)
          putStrLn "Integers (2bit):"
          reportWith sargs  4000 sortI2 (`mapProps` 3000)
          -- By limiting to only 2bit integers, results improve.
          -- Too many possibilities of elements in the lists makes it hard for
          -- FitSpec to find a realistic result.

          putStrLn "Noples:"
          reportWith sargs  1000 sortU (`mapProps`   20)
          putStrLn "Booleans (classification on):"
          report1With csargs 1000 sortB (`mapProps` 1000)
          putStrLn "Integers (classification on):"
          report1With csargs 1000 sortI (`mapProps` 1000)
          putStrLn "Integers (2bit,classification on):"
          report1With csargs 4000 sortI2 (`mapProps` 3000)



sortCounter :: (Bounded a, Ord a) => [a] -> [a]
sortCounter = (++ [maxBound]) . sort
