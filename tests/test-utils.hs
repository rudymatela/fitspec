import System.Exit (exitFailure)
import Data.List (elemIndices,sortBy)

import Data.Function

import Test.LeanCheck
import FitSpec.Utils

main :: IO ()
main =
  case elemIndices False (tests 500) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n =
  [ True

  , sortOn snd [(1,3),(2,2),(3,1)] == [(3,1),(2,2),(1,3)]
  , holds n $ (propSortOn fst :: [(Int,Int)] -> Bool)
  , holds n $ (propSortOn snd :: [(Int,Int)] -> Bool)
  , holds n $ (propSortOn abs :: [Int] -> Bool)
  ]

propSortOn :: (Eq a, Ord b) => (a -> b) -> [a] -> Bool
propSortOn f xs = sortOn f xs == sortBy (compare `on` f) xs
