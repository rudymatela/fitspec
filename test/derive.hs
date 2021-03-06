-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell #-}
import System.Exit (exitFailure)
import Data.List (elemIndices,sort)

import Test.FitSpec hiding (Set)

data D0       = D0                    deriving (Show,Eq,Ord)
data D1 a     = D1 a                  deriving (Show,Eq,Ord)
data D2 a b   = D2 a b                deriving (Show,Eq,Ord)
data D3 a b c = D3 a b c              deriving (Show,Eq,Ord)
data C1 a     =           C11 a | C10 deriving (Show,Eq,Ord)
data C2 a b   = C22 a b | C21 a | C20 deriving (Show,Eq,Ord)
data I a b    = a :+ b                deriving (Show,Eq,Ord)
deriveMutable ''D0
deriveMutable ''D1
deriveMutable ''D2
deriveMutable ''D3
deriveMutable ''C1
deriveMutable ''C2
deriveMutable ''I

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveMutable ''Bool
deriveMutable ''Maybe
deriveMutable ''Either
-}

data Set a = Set [a] deriving (Show,Eq,Ord)

instance (Ord a, Listable a) => Listable (Set a) where
  tiers = setCons Set

deriveMutableE [''Ord] ''Set

main :: IO ()
main =
  case elemIndices False (tests 100) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

type Id a = a -> a

tests n =
  [ True
  , allUnique $ concat $ showNewMutants1 (id :: Id D0) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (D1 UInt2)) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (D2 UInt2 UInt2)) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (D3 UInt2 UInt2 UInt2)) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (C1 UInt2)) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (C2 UInt2 UInt2)) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (I  UInt2 UInt2)) 7
  , allUnique $ concat $ showNewMutants1 (id :: Id (Set UInt2)) 7
  ]

showNewMutants1 :: (ShowMutable a, Mutable a)
                => a -> Int -> [[String]]
showNewMutants1 f n = mapT (showMutantAsTuple [] f)
                    $ take n
                    $ mutiers f

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique lesser
                && allUnique greater
  where lesser  = filter (< x) xs
        greater = filter (> x) xs
