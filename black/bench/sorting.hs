import Data.List

import FitSpec.Main
import Test.Check
import Test.Types.Mutate


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


sargs :: (Show a, Listable a, Bounded a, Ord a)
      => Args ([a] -> [a])
sargs = args
  { callNames = ["sort xs"]
  , minimumTime = 0
  , nMutants = 1000
  , nTestsF = id
  , limitResults = Nothing
  , extraMutants = take 0
                 . concat
                 . lsmap (. sort)
                 $ cons2 (\y ys -> (++ (y:ys))) -- prepend non-empty list
              \++/ cons2 (\y ys -> ((y:ys) ++)) -- append non-empty list
  }

type Ty a = [a] -> [a]

main :: IO ()
main = do
  let run f = mainWith sargs f pmap
  ty <- typeArgument
  case ty of
    "bool"  -> run (sort :: Ty Bool)
    "bools" -> run (sort :: Ty [Bool])
    "int"   -> run (sort :: Ty Int)
    "int2"  -> run (sort :: Ty UInt2)
    "int3"  -> run (sort :: Ty UInt3)
    "unit"  -> run (sort :: Ty ())
    _       -> run (sort :: Ty UInt2)

-- This hack is only necessary when using sortCounter as a manual mutant
instance Bounded a => Bounded [a] where
  minBound = []
  maxBound = repeat maxBound -- non terminating upper bound

sortCounter :: (Bounded a, Ord a) => [a] -> [a]
sortCounter = (++ [maxBound]) . sort
