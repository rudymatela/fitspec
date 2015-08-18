module Main where
import Mutants
import Data.List


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


pmap :: (Ord a, Enumerable a) => Int -> Int -> ([a] -> [a]) -> Bool
pmap n k sort' =
  [ runTests n $ \xs ->            ordered (sort' xs)
  , runTests n $ \xs ->             length (sort' xs) == length xs
  , runTests n $ \(x,xs) ->         elem x (sort' xs) == elem x xs
  , runTests n $ \(x,xs) ->        count x (sort' xs) == count x xs
  , runTests n $ \xs ->     permutation xs (sort' xs)
  , runTests n $ \xs ->              sort' (sort' xs) == sort' xs
  , runTests n $ \(x,xs) ->       insert x (sort' xs) == sort' (x:xs)
  ] !! k

pmap' :: (Ord a, Enumerable a) => Int -> [([a] -> [a]) -> Bool]
pmap' n = map (pmap n) [0..6]

sortI :: [Int] -> [Int]
sortI = sort

sortB :: [Bool] -> [Bool]
sortB = sort

sortU :: [()] -> [()]
sortU = sort

main :: IO ()
main = do runM 9 (pmap' 4000) (sort :: [Bool] -> [Bool])
      --    runV (Just $ valid 4000) 10 (pmap' 4000) (sort :: [[Bool]] -> [[Bool]])

valid :: Enumerable a => Int -> ([a] -> [a]) -> ()
valid n f = v f where
  v a = runTests n ((`seq` True) . a) `seq` ()


