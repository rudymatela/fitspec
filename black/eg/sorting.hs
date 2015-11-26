import Test.Check
import Test.Types
import Test.Types.Mutate
import FitSpec
import Data.List

ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x<=y && ordered (y:xs)

-- Given the number of tests and a sorting implementation
-- return whether each property holds
pmap :: (Show a, Ord a, Listable a) => Int -> ([a] -> [a]) -> [Bool]
pmap n sort =
  [ holds n $ \xs   -> ordered (sort xs)
  , holds n $ \xs   -> length (sort xs) == length xs
  , holds n $ \x xs -> elem x xs == elem x (sort xs)
  , holds n $ \x xs -> count x xs == count x (sort xs)
  ]
  where count x = length . filter (==x)

main = do
  reportWith args { callNames = ["sort xs"]
                  , minimumTime = 30 -- or 0, and:
               -- , nMutants = 4000
                  , nTestsF = (*1)
                  , limitResults = Just 3
                  }
             (sort::[UInt2]->[UInt2])
             pmap
