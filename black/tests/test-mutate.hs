import System.Exit (exitFailure)
import Data.List (elemIndices, sort)

import Test.Check
import Test.Check.Utils

import Mutate

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure


tests =
  [ True

  , lsMutantsEqOld (sort :: [Int]  -> [Int]) 5
  , lsMutantsEqOld (sort :: [Bool] -> [Bool]) 5
  , lsMutantsEqOld (sort :: [Char] -> [Char]) 5
-- TODO: Find out why and when the following loops:
--, lsMutantsEqOld (sort :: [()] -> [()]) 5

  , lsMutantsEqOld (uncurry (++) :: ([Int],[Int]) -> [Int]) 4
  , lsMutantsEqOld (uncurry (++) :: ([Bool],[Bool]) -> [Bool]) 4
  , lsMutantsEqOld (uncurry (++) :: ([Char],[Char]) -> [Char]) 4
  ]


lsMutantsEqOld :: ( Show a, Show b
                  , Eq a, Eq b
                  , Listable a, Listable b
                  , Mutable b)
               => (a -> b) -> Int -> Bool
lsMutantsEqOld f n = oldMutants == newMutants
  where oldMutants = lsmap (showMutant f) $ take n $ lsMutantsOld f
        newMutants = lsmap (showMutant f) $ take n $ szMutants f


lsMutantsOld :: (Eq a, Eq b, Listable a, Listable b)
             => (a -> b) -> [[a -> b]]
lsMutantsOld f = lsmap (defaultFunPairsToFunction f)
               $ lsfilter (canonicalMutation f)
               $ lsFunctionPairs listing listing
