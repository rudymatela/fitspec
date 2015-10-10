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

nOccurrences :: Eq a => a -> [a] -> Int
nOccurrences x = length . filter (==x)

unsuspectingProps :: (Ord a, Show a, Listable a) => ([a] -> [a]) -> Int -> [Bool]
unsuspectingProps sort' n =
  [ holds n $ \xs ->           length (sort' xs) == length xs
  , holds n $ \xs ->          ordered (sort' xs)
  , holds n $ \x xs ->         elem x (sort' xs) == elem x xs
  , holds n $ \x xs ->   (not.elem x) (sort' xs) == (not.elem x) xs
  ]


completeProps :: (Ord a, Show a, Listable a) => ([a] -> [a]) -> Int -> [Bool]
completeProps sort' n =
  [ holds n $ \xs ->          ordered (sort' xs)
  , holds n $ \x xs -> nOccurrences x (sort' xs) == nOccurrences x xs
  , holds n $ \xs ->   permutation xs (sort' xs)
  ]

sortI :: [Int] -> [Int]
sortI = sort

sortB :: [Bool] -> [Bool]
sortB = sort

sargs = args { callNames = ["sort xs"]
             }

main :: IO ()
main = do putStrLn "### Unsuspecting Properties ###"
          putStrLn "Over booleans:"
          reportWith sargs 2000 sortB (`unsuspectingProps` 4000)
          putStrLn "Over integers:"
          reportWith sargs 4000 sortI (`unsuspectingProps` 10000)

          putStrLn "### Complete (but not minimal) Properties ###"
          putStrLn "Over booleans:"
          reportWith sargs 2000 sortB (`completeProps` 4000)
          putStrLn "Over integers:"
          reportWith sargs 4000 sortI (`completeProps` 100000)

