import System.Exit (exitFailure)
import Data.List (elemIndices, sort)

import Test.Check
import Test.Check.Utils
import Utils (errorToNothing,contained)
import Data.Tuple (swap)

import Test.Types
import Test.Types.Mutate

import Mutate
import Mutate.Show


main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

-- NOTE:
-- the lsMutantsEqOld property only *actually* hold for functions returning
-- lists when using lsMutantsEq as the implementation of lsMutants for [a]

tests =
  [ True

  , lsMutantsEqOld (sort :: [Int]  -> [Int]) 5
  , lsMutantsEqOld (sort :: [Bool] -> [Bool]) 3 -- was 5
  , lsMutantsEqOld (sort :: [Char] -> [Char]) 5
-- TODO: Find out why and when the following does not terminate:
--, lsMutantsEqOld (sort :: [()] -> [()]) 5

  , lsMutantsEqOld (head :: [Int] -> Int) 6
  , lsMutantsEqOld (head :: [Bool] -> Bool) 6
  , lsMutantsEqOld (tail :: [Int] -> [Int]) 6
  , lsMutantsEqOld (tail :: [Bool] -> [Bool]) 4 -- was 6

  , lsMutantsEqOld (uncurry (++) :: ([Int],[Int]) -> [Int]) 4
  , lsMutantsEqOld (uncurry (++) :: ([Bool],[Bool]) -> [Bool]) 4
  , lsMutantsEqOld (uncurry (++) :: ([Char],[Char]) -> [Char]) 4

  , lsMutantsEqOld not 10 
  , lsMutantsEqOld (uncurry (&&)) 10
  , lsMutantsEqOld (uncurry (||)) 10

  , lsMutantsEqOld (uncurry (+) :: (Int,Int) -> Int) 6
  , lsMutantsEqOld (uncurry (+) :: (Nat,Nat) -> Nat) 6
  , lsMutantsEqOld (uncurry (*) :: (Int,Int) -> Int) 6
  , lsMutantsEqOld (uncurry (*) :: (Nat,Nat) -> Nat) 6

  -- These actually do not hold for later values in the enumeration
  -- The actual way in which values are enumerated makes the enumerations
  -- inherently different.
  , lsMutants2EqOld ((++) :: [Int] -> [Int] -> [Int]) 4
  , lsMutants2EqOld ((++) :: [Bool] -> [Bool] -> [Bool]) 3
  , lsMutants2EqOld ((++) :: [Char] -> [Char] -> [Char]) 4

  , allUnique $ concat $ showOldMutants1 (sort :: [Int] -> [Int]) 7
  , allUnique $ concat $ showNewMutants1 (sort :: [Int] -> [Int]) 7
  , allUnique $ concat $ showOldMutants2 ((++) :: [Int] -> [Int] -> [Int]) 7
  , allUnique $ concat $ showNewMutants2 ((++) :: [Int] -> [Int] -> [Int]) 7

  , allUnique $ concat $ showOldMutants1 (swap :: (Int,Int) -> (Int,Int)) 7
  , allUnique $ concat $ showNewMutants1 (swap :: (Int,Int) -> (Int,Int)) 7
  , allUnique $ concat $ showOldMutants1 (swap :: (Bool,Bool) -> (Bool,Bool)) 7
  , allUnique $ concat $ showNewMutants1 (swap :: (Bool,Bool) -> (Bool,Bool)) 7

  , allUnique $ concat $ showOldMutants2 ((,) :: Int -> Bool -> (Int,Bool)) 7
  , allUnique $ concat $ showNewMutants2 ((,) :: Int -> Bool -> (Int,Bool)) 7
  , allUnique $ concat $ showNewMutants2 ((,) :: Bool -> Int -> (Bool,Int)) 7

  , checkBindingsOfLength 7 2 ((,) :: Bool -> Bool -> (Bool,Bool))
  , checkBindingsOfLength 7 2 ((,) :: Int -> Int -> (Int,Int))
  , checkBindingsOfLength 7 1 (swap :: (Bool,Bool) -> (Bool,Bool))
  , checkBindingsOfLength 4 1 (swap :: (Bool,Bool) -> (Bool,Bool),sort :: [Int] -> [Int])

  , holds 25 (uniqueMutants    100 :: [Bool] -> Bool)
  , holds 25 (mutantsInListing 100 :: [Bool] -> Bool)
  , holds 25 (listingInMutants 100 :: [Bool] -> Bool)
  , holds 25 (uniqueMutants    100 :: [Int] -> Bool)
  , holds 25 (mutantsInListing 100 :: [Int] -> Bool)
  , holds 25 (listingInMutants 100 :: [Int] -> Bool)
  , holds 25 (uniqueMutants    100 :: [()] -> Bool)
  , holds 25 (mutantsInListing 100 :: [()] -> Bool)
  , holds 25 (listingInMutants 100 :: [()] -> Bool)
  , holds 25 (uniqueMutants    100 :: Bool -> Bool)
  , holds 25 (mutantsInListing 100 :: Bool -> Bool)
  , holds 25 (listingInMutants 100 :: Bool -> Bool)
  , holds 25 (uniqueMutants    100 :: Int -> Bool)
  , holds 25 (mutantsInListing 100 :: Int -> Bool)
  , holds 25 (listingInMutants 100 :: Int -> Bool)
  , holds 25 (uniqueMutants    100 :: () -> Bool)
  , holds 25 (mutantsInListing 100 :: () -> Bool)
  , holds 25 (listingInMutants 100 :: () -> Bool)
  ]


uniqueMutants :: (Ord a, Listable a, Mutable a) => Int -> a -> Bool
uniqueMutants n = allUnique . take n . mutants

mutantsInListing :: (Eq a, Listable a, Mutable a) => Int -> a -> Bool
mutantsInListing n x = take n (mutants x) `contained` list

listingInMutants :: (Eq a, Listable a, Mutable a) => Int -> a -> Bool
listingInMutants n x = take n list        `contained` mutants x


checkBindingsOfLength :: (Mutable a, ShowMutable a)
                      => Int -> Int -> a -> Bool
checkBindingsOfLength n len f = (all . all) (bindingsOfLength len)
                              . concat
                              . take n
                              . lsmap (mutantS f)
                              $ szMutants f


bindingsOfLength :: Int -> [([String],String)] -> Bool
bindingsOfLength n = all ((== n) . length . fst)


lsMutantsEqOld :: ( Show a, Show b
                  , Eq a, Eq b
                  , Listable a, Listable b
                  , Mutable b, ShowMutable b )
               => (a -> b) -> Int -> Bool
lsMutantsEqOld f n = showOldMutants1 f n == showNewMutants1 f n


lsMutants2EqOld :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Listable c
                   , Mutable c, ShowMutable b, ShowMutable c )
                => (a -> b -> c) -> Int -> Bool
lsMutants2EqOld f n = showOldMutants2 f n == showNewMutants2 f n


showOldMutants1 :: ( Eq a, Eq b
                   , Show a, Show b
                   , Listable a, Listable b
                   , ShowMutable b )
                => (a -> b) -> Int -> [[String]]
showOldMutants1 f n = lsmap (showMutant f)
                    $ take n
                    $ lsMutantsOld f

showNewMutants1 :: (ShowMutable a, Mutable a)
                => a -> Int -> [[String]]
showNewMutants1 f n = lsmap (showMutant f)
                    $ take n
                    $ szMutants f

showOldMutants2 :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Listable c
                   , ShowMutable c )
                => (a -> b -> c) -> Int -> [[String]]
showOldMutants2 f = showOldMutants1 (uncurry f)

showNewMutants2 :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Mutable c
                   , ShowMutable c )
                => (a -> b -> c) -> Int -> [[String]]
showNewMutants2 f n = lsmap (showMutant uf . uncurry)
                    $ take n
                    $ szMutants f
  where uf = uncurry f

lsMutantsOld :: (Eq a, Eq b, Listable a, Listable b)
             => (a -> b) -> [[a -> b]]
lsMutantsOld f = lsmap (defaultFunPairsToFunction f)
               $ lsfilter (canonicalMutation f)
               $ lsFunctionPairs listing listing

canonicalMutation :: Eq b => (a -> b) -> [(a, b)] -> Bool
-- This simple version on the line below
-- is one that does not deal with partially undefined functions.
-- canonicalMutation f = all (\(a,r) -> f a /= r)
canonicalMutation f = all different
  where
    -- the errorToNothing here deals partial functions (error/undefined)
    -- We define that mutating undefined values is noncanonical
    different (a,r) = case errorToNothing $ f a of
                        Just r' -> r' /= r
                        Nothing -> False -- for our purposes,
                                         -- undefined is equal to anything

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (lesser)
                && allUnique (greater)
  where lesser  = filter (< x) xs
        greater = filter (> x) xs
