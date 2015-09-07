{-# Language DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs hiding (args)
import Mutants
import Data.List


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


pmap :: (Ord a, Enumerable a) => Int -> Int -> ([a] -> [a]) -> Bool
pmap n k sort' =
  [ runTests n $ \xs ->            ordered (sort' xs)
  , runTests n $ \xs ->             length (sort' xs) == length xs
  , runTests n $ \(x,xs) ->         elem x (sort' xs) == elem x xs
  , runTests n $ \(x,xs) ->        count x (sort' xs) == count x xs
  , runTests n $ \xs ->     permutation xs (sort' xs)
  , runTests n $ \xs ->              sort' (sort' xs) == sort' xs
  , runTests n $ \(x,xs) ->       insert x (sort' xs) == sort' (x:xs)
  ] !! k

pmap' :: (Ord a, Enumerable a) => Int -> [([a] -> [a]) -> Bool]
pmap' n = map (pmap n) [0..6]

sortI :: [Int] -> [Int]
sortI = sort

sortB :: [Bool] -> [Bool]
sortB = sort

sortU :: [()] -> [()]
sortU = sort

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , typeLevelMutants :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 4000    &= help "number of tests to run"
  , nMutants = 9       &= help "mutant threshold"
                       &= name "m"
  , testType = "bool"  &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , typeLevelMutants = False
                       &= help "use type level mutants (mutate output of functions)"
                       &= name "y"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (typeLevelMutants as) (nMutants as) (nTests as)
       -- runM (nMutants as) (pmap' (nTests as)) (sort :: [Bool] -> [Bool])
       -- runV (Just $ valid 4000) 10 (pmap' 4000) (sort :: [[Bool]] -> [[Bool]])

type Ty a = [a] -> [a]

run :: String -> Bool -> Int -> Int -> IO ()
run "int"   = run' (sort :: Ty Int)
run "bool"  = run' (sort :: Ty Bool)
run "bools" = run' (sort :: Ty [Bool])

run' f True  nMutants nTests = runM nMutants (pmap' nTests) f
run' f False nMutants nTests = runV (Just $ valid nTests) nMutants (pmap' nTests) f

valid :: Enumerable a => Int -> ([a] -> [a]) -> ()
valid n f = v f where
  v a = runTests n ((`seq` True) . a) `seq` ()


