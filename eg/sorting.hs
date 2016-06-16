-- Minimal example: mutation testing a sort specification
--
-- (this program might take a minute to run depending on your system)
--
-- Usage:
--
-- $ ghc -O2 sorting.hs
-- $ ./sorting
-- Apparent incomplete and non-minimal specification based on
-- 4000 test cases for each of properties 1, 2, 3, 4 and 5
-- for each of 4000 mutant variations.
--
-- 3 survivors (99% killed), smallest:
--   \xs -> case xs of
--            [0,0,1] -> [0,1,1]
--            _ -> sort xs
--
-- apparent minimal property subsets:  {1,2,3} {1,2,4}
-- conjectures:  {3}    =  {4}     96% killed (weak)
--               {1,3} ==> {5}     98% killed (weak)

import FitSpec
import Data.List (sort)

properties sort =
  [ property $ \xs   -> ordered (sort xs)
  , property $ \xs   -> length (sort xs) == length xs
  , property $ \x xs -> elem x (sort xs) == elem x xs
  , property $ \x xs -> notElem x (sort xs) == notElem x xs
  , property $ \x xs -> minimum (x:xs) == head (sort (x:xs))
  ]
  where
  ordered (x:y:xs) = x <= y && ordered (y:xs)
  ordered _        = True

main =
  mainWith args { names = ["sort xs"]
                , nMutants = 4000
                , nTests   = 4000
                , timeout  = 0
                }
           (sort::[Word2]->[Word2])
           properties
