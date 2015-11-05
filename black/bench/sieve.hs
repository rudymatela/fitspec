-- Example benchmark that mutation tests properties over an infinite list of primes
{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
import Test.Check
import Test.Types
import Test.Types.Mutate
import Data.Maybe

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
                      LT -> x `elemO` xs
                      EQ -> True
                      GT -> False
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
  , holds n $ \x -> (x >= 2 && x `elemO` primes) ==> x*x `notElemO` primes
  ]


sargs :: Args [Int]
sargs = args { limitResults = Just 10
             , showMutantN = \_ _ -> showInfinite
             }
  where showInfinite xs | not . null $ drop 10 xs = (init . show $ take 10 xs) ++ "..."
                        | otherwise                 = show xs

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , classify :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments :: CmdArguments
arguments = CmdArguments
  { nTests   = 1000    &= help "number of tests to run"
  , nMutants = 1000    &= help "number of mutants to generate"
                       &= name "m"
  , classify = False   &= help "classify mutants, report extra column with fully evaluated ones (grey-box)"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (classify as) (nMutants as) (nTests as)

run :: Bool -> Int -> Int -> IO ()
run False nm nt = reportWith sargs nm primes (pmap nt)
run True  nm nt = undefined
