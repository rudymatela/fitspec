import FitSpec
import Data.List

ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x<=y && ordered (y:xs)

-- Given the number of tests and a sorting implementation
-- return whether each property holds
properties :: (Show a, Ord a, Listable a) => ([a] -> [a]) -> [Property]
properties sort =
  [ property $ \xs   -> ordered (sort xs)
  , property $ \xs   -> length (sort xs) == length xs
  , property $ \x xs -> elem x xs == elem x (sort xs)
  , property $ \x xs -> count x xs == count x (sort xs)
  ]
  where count x = length . filter (==x)

main =
  reportWith args { names = ["sort xs"]
                  , nMutants = 2000
                  , nTests   = 2000
                  , timeout  = 30
                  , limitResults = Just 3
                  }
             (sort::[UInt2]->[UInt2])
             properties
