
import Control.Enumerable
import Control.Enumerable.Functions
import Control.Enumerable.Values
import Control.Enumerable.Count

import System.IO.Unsafe
import Data.IORef
import Control.Monad(when)
import Data.Maybe(isJust)
-- import Data.Either(partitionEithers)

import Data.List (sort) -- Just for testing

attach :: Eq b => (a -> b) -> (a -> b) -> IO (IO Bool, a -> b)
attach original mutant = do
  r <- newIORef False
  let f a = let b = mutant a 
            in unsafePerformIO $ do
                when (b /= original a) (writeIORef r True)
                return b 
  return (readIORef r, f)


test1 f m xs = do 
  (mb, f') <- attach f m
  print (map f' xs)
  mb >>= print
  


newtype FreeBool = Free {freeBool :: Bool}
instance Enumerable FreeBool where
  enumerate = share $ fmap Free (pure False <|> pure True)

patterns :: Parameter a => Int -> (Integer, [Function a Bool])
patterns k = (crds, map (fmap freeBool) vs) where
  crds = sum $ take (k+1) $ count $ countparam vs
  vs = concat $ values' k

-- | Returns a minimal pattern (up to a given size limit) that distinguishes the elements of two lists
partition :: Parameter a => Int -> [a] -> [a] -> Function a Bool 
partition k xs ys = head [ p  |p <- snd (patterns k)
                                    , not (any (p $$) xs)
                                    , any (p $$) ys
                                    ]



type Mutator a b = Function a (Maybe b)

proper :: Mutator a b -> Bool
proper = any isJust . rhss

mutants :: (Parameter a, Enumerable b) => Int -> (Integer, [Mutator a b])
mutants k = (crds, vs) where
  crds = sum $ take (k+1) $ count $ countparam vs
  vs = concat $ values' k

propers k = filter proper $ snd $ mutants k

minis k = tail $ filter mini $ snd $ mutants k

testmutants k = mutants k :: (Integer, [Mutator [Bool] [Bool]])
testpropers k = propers k :: [Mutator [Bool] [Bool]]
testminis   k = minis   k :: [Mutator [Bool] [Bool]]

overlay :: (a -> b) -> (a -> Maybe b) -> (a -> b)
overlay f g a = maybe (f a) id (g a)

mutate :: Eq b => (a -> b) -> Mutator a b -> IO (IO Bool, (a -> b))
mutate f m = attach f (overlay f (m $$))

findMutant :: (Parameter a, Enumerable b, Eq b, Show b) => 
                 Int -> (a -> b) -> ((a -> b) -> Bool) -> IO ()
findMutant k f p = do
  putStrLn $ "Killing "++show n++" mutants"
  putStrLn $ show n'++" proper mutants\n"
  go ps
  where
    (n,ms) = mutants k
    n' = length ps
    ps = tail $ filter mini $ ms
    go [] = putStrLn "All mutants are dead."
    go (x:xs) = do
      (mb, f') <- mutate f x
      case p f' of 
        False -> go xs
        True  -> do
          used <- mb
          if used  then putStrLn "One got through!" >> print x
                   else putStrLn "Killed!" >> print x >> putStrLn "" >> 
                        go xs

runTests :: Enumerable x => Int -> (x -> Bool) -> Bool
runTests n p = all p (take n (concatMap values [0..]))

prepost :: Enumerable a => Int -> (a -> b) -> (a -> b -> Bool) -> Bool
prepost n f p = runTests n $ \a -> p a (f a)



-- Example 1
ordered :: [Bool] -> Bool
ordered (x:t@(y:_)) = x <= y && ordered t
ordered _           = True

matchLength :: [Bool] -> [Bool] -> Bool
matchLength xs ys = length xs == length ys


test_orders :: Int -> ([Bool] -> [Bool]) -> Bool
test_orders n f = prepost n f $ \i o -> ordered o && matchLength i o

example1 k n = findMutant k sort (test_orders n) -- Parameters: max-size of mutants and number of tests



-- Example 2
append = uncurry (++)

assoc :: Int -> (([Bool],[Bool]) -> [Bool]) -> Bool
assoc k ap = let (+) = curry ap 
           in runTests k $ \(x,y,z) -> x + (y + z) == (x + y) + z


example2 k n = findMutant k append (assoc n)

