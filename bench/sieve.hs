-- Example benchmark that mutation tests properties over an infinite list of primes
import System.Console.CmdArgs hiding (args)
import FitSpec
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
                      LT -> False        -- would already have appeared int the list
                      EQ -> True
                      GT -> x `elemO` xs
infix 4 `elemO`

notElemO :: Ord a => a -> [a] -> Bool
notElemO = (not .) . elemO
infix 4 `notElemO`

-- The property map
properties :: [Int] -> [Property]
properties primes =
  [ property $ listToMaybe primes == Just 2 -- start with two
--, property $ length (take n primes) == n  -- infinite
--, property $ allUnique (take n primes)
--, property $ strictlyOrdered (take n primes)
  , property $ \x -> x `elemO` primes
                 ==> x*x `notElemO` primes
  , property $ \x y -> x `elemO` primes
                    && y `elemO` primes
                   ==> x*y `notElemO` primes
--, property $ \i' -> let i  = fromIntegral (i'::Nat)
--                        p  = primes !! i
--                        ps = drop (i+1) primes
--                    in  p > 1 ==> all (\p' -> p' `mod` p /= 0) (take n ps)
  , property $ \x y -> x `elemO` primes
                    && y `elemO` primes
                    && x /= y
                   ==> x `mod` y /= 0
--, all prime            (take n primes)              -- sound
--, all (`elemO` primes) [ x | x <- [1..n], prime x ] -- complete
  ]
  where prime x = x > 1
               && all (\p -> p > 0 && x `mod` p /= 0)
                      (takeWhile (\p -> p*p <= x) primes)


prime x = x > 1 && all (\p -> p `mod` x /= 0) (takeWhile (\p -> p*p <= x) primes)

sargs :: Args [Int]
sargs = args
  { limitResults = Just 10
  , showMutantN = \_ _ -> showInfinite
  , nMutants = 20000
  , nTestsF = const 100
  , minimumTime = 0
  }
  where showInfinite xs | not . null $ drop 10 xs = (init . show $ take 10 xs) ++ "..."
                        | otherwise               = show xs

main :: IO ()
main = mainWith sargs primes properties

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique lesser
                && allUnique greater
  where lesser  = filter (< x) xs
        greater = filter (> x) xs
