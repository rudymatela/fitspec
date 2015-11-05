-- Example benchmark that mutation tests properties over an infinite list of primes
{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
import Test.Check
import Test.Types
import Test.Types.Mutate
import Data.Maybe
import Utils (errorToFalse)

-- The code under test
primes :: [Int]
primes = sieve [2..]
  where sieve (p:ns) = p : sieve [n | n <- ns, n `mod` p /= 0]

-- Some auxiliary functions
strictlyOrdered [] = True
strictlyOrdered [x] = True
strictlyOrdered (x:y:xs) = x<y && strictlyOrdered (y:xs)

elemO :: Ord a => a -> [a] -> Bool
x `elemO` [] = False
x `elemO` (x':xs) = case x `compare` x' of
                      LT -> False        -- would already have appeared int the list
                      EQ -> True
                      GT -> x `elemO` xs
infix 4 `elemO`

notElemO :: Ord a => a -> [a] -> Bool
notElemO = (not .) . elemO
infix 4 `notElemO`

-- The property map
pmap :: Int -> [Int] -> [Bool]
pmap n primes =
  [ listToMaybe primes == Just 2 -- start with two
  , length (take n primes) == n  -- infinite
  , strictlyOrdered (take n primes)
  , holds n $ \x -> x `elemO` primes
                ==> x*x `notElemO` primes
  , holds n $ \x y -> x `elemO` primes
                   && y `elemO` primes
                  ==> x*y `notElemO` primes
  , holdE n $ \i' -> let i  = fromIntegral (i'::Nat)
                         p  = primes !! i
                         ps = drop (i+1) primes
                     in  p > 1 ==> all (\p' -> p' `mod` p /= 0) (take n ps)
  , allUnique (take n primes)
  ]
  where holdE n = errorToFalse . holds n


sargs :: Args [Int]
sargs = args { limitResults = Just 10
             , showMutantN = \_ _ -> showInfinite
             }
  where showInfinite xs | not . null $ drop 10 xs = (init . show $ take 10 xs) ++ "..."
                        | otherwise               = show xs

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , classify :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments :: CmdArguments
arguments = CmdArguments
  { nTests   = 100     &= help "number of tests to run"
  , nMutants = 20000   &= help "number of mutants to generate"
                       &= name "m"
  , classify = False   &= help "classify mutants, report extra column with fully evaluated ones (grey-box)"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (classify as) (nMutants as) (nTests as)

run :: Bool -> Int -> Int -> IO ()
run False nm nt = reportWith sargs nm primes (pmap nt)
run True  nm nt = undefined

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (lesser)
                && allUnique (greater)
  where lesser  = filter (< x) xs
        greater = filter (> x) xs
