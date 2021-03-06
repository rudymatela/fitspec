-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import System.Exit (exitFailure)
import Data.List (elemIndices, sort)
import Data.Tuple (swap)

import Test.FitSpec
import Test.FitSpec.Utils (contained)
import Test.LeanCheck.Error (errorToNothing, errorToFalse)

import Data.Monoid ((<>))
import Data.Word (Word) -- for GHC <= 7.10

polyAppend :: [a] -> [b] -> [Either a b]
polyAppend xs ys = map Left xs ++ map Right ys

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests = map errorToFalse
  [ True

  , allUnique $ concat $ showNewMutants1 (sort :: [Int] -> [Int]) 7
  , allUnique $ concat $ showNewMutants2 ((++) :: [Int] -> [Int] -> [Int]) 7

  , allUnique $ concat $ showNewMutants1 (swap :: (Int,Int) -> (Int,Int)) 7
  , allUnique $ concat $ showNewMutants1 (swap :: (Bool,Bool) -> (Bool,Bool)) 7

  , allUnique $ concat $ showNewMutants2 ((,) :: Int -> Bool -> (Int,Bool)) 7
  , allUnique $ concat $ showNewMutants2 ((,) :: Bool -> Int -> (Bool,Int)) 7

  , allUnique $ concat $ showNewMutants2 (polyAppend :: [String] -> [Int] -> [Either String Int]) 7
  , allUnique $ concat $ showNewMutants2 ((+) :: Float -> Float -> Float) 7
  , allUnique $ concat $ showNewMutants2 ((+) :: Double -> Double -> Double) 7
  , allUnique $ concat $ showNewMutants2 ((<>) :: Ordering -> Ordering -> Ordering) 7
  , allUnique $ concat $ showNewMutants2 ((+) :: Word -> Word -> Word) 7

  {-
  , checkBindingsOfLength 7 2 ((,) :: Bool -> Bool -> (Bool,Bool))
  , checkBindingsOfLength 7 2 ((,) :: Int -> Int -> (Int,Int))
  , checkBindingsOfLength 7 1 (swap :: (Bool,Bool) -> (Bool,Bool))
  , checkBindingsOfLength 4 1 (swap :: (Bool,Bool) -> (Bool,Bool),sort :: [Int] -> [Int])
  -}

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


{- does not work as for the new interface for mutantS
checkBindingsOfLength :: (Mutable a, ShowMutable a)
                      => Int -> Int -> a -> Bool
checkBindingsOfLength n len f = (all . all) (bindingsOfLength len)
                              . concat
                              . take n
                              . mapT (mutantS f)
                              $ mutiers f
-}


bindingsOfLength :: Int -> [([String],String)] -> Bool
bindingsOfLength n = all ((== n) . length . fst)


showNewMutants1 :: (ShowMutable a, Mutable a)
                => a -> Int -> [[String]]
showNewMutants1 f n = mapT (showMutantAsTuple [] f)
                    $ take n
                    $ mutiers f

showNewMutants2 :: ( Eq a, Eq b, Eq c
                   , Show a, Show b, Show c
                   , Listable a, Listable b, Mutable c
                   , ShowMutable c )
                => (a -> b -> c) -> Int -> [[String]]
showNewMutants2 f n = mapT (showMutantAsTuple [] uf . uncurry)
                    $ take n
                    $ mutiers f
  where uf = uncurry f

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
                && allUnique lesser
                && allUnique greater
  where lesser  = filter (< x) xs
        greater = filter (> x) xs
