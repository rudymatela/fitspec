import FitSpec
import Data.List
import Test.Check


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

main :: IO ()
main = do putStrLn "### Strict mutant enumerations ###"
          putStrLn "Booleans:"
          reportWith sargs  1000 sortB (`mapProps` 1000)
          putStrLn "Integers:"
          reportWith sargs  1000 sortI (`mapProps` 1000)
          putStrLn "Noples:"
          reportWith sargs  1000 sortU (`mapProps`   20)


sortCounter :: (Bounded a, Ord a) => [a] -> [a]
sortCounter = (++ [maxBound]) . sort

