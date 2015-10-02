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

  , lsMutants2EqOld ((++) :: [Int] -> [Int] -> [Int]) 4
  , lsMutants2EqOld ((++) :: [Bool] -> [Bool] -> [Bool]) 4
  , lsMutants2EqOld ((++) :: [Char] -> [Char] -> [Char]) 4
  ]


lsMutantsEqOld :: ( Show a, Show b
                  , Eq a, Eq b
                  , Listable a, Listable b
                  , Mutable b )
               => (a -> b) -> Int -> Bool
lsMutantsEqOld f n = showOldMutants1 f n == showNewMutants1 f n


lsMutants2EqOld :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Listable c
                   , Mutable c )
                => (a -> b -> c) -> Int -> Bool
lsMutants2EqOld f n = showOldMutants2 f n == showNewMutants2 f n


showOldMutants1 :: ( Eq a, Eq b
                   , Show a, Show b
                   , Listable a, Listable b )
                => (a -> b) -> Int -> String
showOldMutants1 f n = unlines
                    $ map concat
                    $ lsmap (showMutant f)
                    $ take n
                    $ lsMutantsOld f

showNewMutants1 :: (ShowMutable a, Mutable a)
                => a -> Int -> String
showNewMutants1 f n = unlines
                    $ map concat
                    $ lsmap (showMutant f)
                    $ take n
                    $ szMutants f

showOldMutants2 :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Listable c )
                => (a -> b -> c) -> Int -> String
showOldMutants2 f = showOldMutants1 (uncurry f)

-- TODO: Remove flip occurrence on showNewMutants2
-- make enumeration actually consistent with tupled functions.
-- A similar thing happened somewhere on llcheck I think, where the
-- left-right-handedness of lists was different from tuples (I had to use
-- reverse in that case).  That is also still not fixed as of 2015-10-02.
--
-- NOTE: The return values are also affected in the enumeration order.
-- Maybe it is easier to fix the llcheck tuple/list consistency problem?
showNewMutants2 :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Mutable c )
                => (a -> b -> c) -> Int -> String
showNewMutants2 f n = unlines
                    $ map concat
                    $ lsmap (showMutant uf . uncurry . flip)
                    $ take n
                    $ szMutants (flip f)
  where uf = uncurry f

lsMutantsOld :: (Eq a, Eq b, Listable a, Listable b)
             => (a -> b) -> [[a -> b]]
lsMutantsOld f = lsmap (defaultFunPairsToFunction f)
               $ lsfilter (canonicalMutation f)
               $ lsFunctionPairs listing listing
