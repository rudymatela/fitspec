module Mutants where
import Control.Enumerable
import Control.Enumerable.Functions
import Control.Enumerable.Values
import Control.Enumerable.Count

import System.IO.Unsafe
import Data.IORef
import Control.Monad(when)
import Data.Maybe(isJust, isNothing)
-- import Data.Either(partitionEithers)

import Data.List (sortBy, sort)
import Data.Ord (comparing)
import Data.Map(Map, insertWith, toList, empty, delete)


attach :: Eq b => (a -> b) -> (a -> Maybe b) -> IO (IO (Bool,Bool), a -> b)
attach original mutant = do
  r <- newIORef False -- Has the function been executed on a modified value
  s <- newIORef False -- Has the function been executed
  let f a = maybe (original a) (x a) (mutant a) 
      x a b = unsafePerformIO $ do
                writeIORef s True
                when (b /= original a) (writeIORef r True)
                return b
      m = do
        x <- readIORef r
        y <- readIORef s
        -- writeIORef r False
        -- writeIORef s False
        return (x,y)
  return (m, f)




type Mutator a b = Function a (Maybe b)

proper :: Mutator a b -> Bool
proper = any isJust . rhss

mutants :: (Parameter a, Enumerable b) => Int -> (Integer, [Mutator a b])
mutants k = (crds, vs) where
  crds = sum $ take (k+1) $ count $ countparam vs
  vs = concat $ values' k

-- |Minimal, proper mutants (unless they accidentally mirror the original function)
minis k = tail $ filter mini $ snd $ mutants k

mutate :: Eq b => (a -> b) -> Mutator a b -> IO (IO (Bool,Bool), (a -> b))
mutate f m = attach f (m $$)


data Outcome = Survived
             | Killed
             | Unmatched
             | Unchanged
             deriving (Eq, Ord ,Show)

useMutant :: Eq b => ((a -> b) -> Bool) -> (a -> b) -> Mutator a b -> IO Outcome
useMutant p f m = do
  (mb,f') <- mutate f m
  case p f' of 
    True   -> mb >>= \(proper,used) -> case (proper,used) of
                      (False,False)    -> return Unmatched
                      (True,True)      -> return Survived
                      (False,True)     -> return Unchanged
    False  -> return Killed
  

useMutant' :: Eq b => (a -> b) -> ((a -> b) -> Bool) -> Mutator a b -> Outcome
useMutant' f p m = unsafePerformIO (useMutant p f m) -- This is probably safe, right?


runTests :: Enumerable x => Int -> (x -> Bool) -> Bool
runTests n p = all p (take n (concatMap values [0..]))

prepost :: Enumerable a => Int -> (a -> b) -> (a -> b -> Bool) -> Bool
prepost n f p = runTests n $ \a -> p a (f a)



type PropertyID     = Int
type PropertySet a  = [(PropertyID, a -> Outcome)]
data Result a = Result 
  { survivors :: Map [PropertyID] (a,Int,Int) -- Maps property subsets to minimal survivor, and number of survivors divided into actual/uncovered survivors
  , improper  :: Int
--  , killed    :: Int
  }


result :: [(PropertyID,Outcome)] -> a -> Result a -> Result a
result ps a r
  | Survived `elem` os       = rep 1 0  -- Actual survivor
  | Killed `elem` os         = rep 0 1  -- Failure to match on subset
  | Unchanged `elem` os      = r{improper = improper r + 1} -- Either kill or survived, later
  | otherwise                = rep 0 1  -- Failure to match on complete set
  where pids  = [ pid | (pid,o) <- ps, o /= Killed]
        
        os                        = map snd ps
        merge (new,n,x) (old,m,y) = (if m == 0 && n > 0 then new else old,n+m,x+y)
        rep n x                   = r{survivors = insertWith merge pids (a,n,x) (survivors r)}

framework :: [a] -> PropertySet a -> Result a
framework ms ps = go ms Result{survivors = Data.Map.empty, improper = 0} where
  go []      r  = r
  go (x:xs)  r  = go xs $ result [(pid,p x) | (pid,p) <- ps ] x r



report :: Show a => Result a -> IO ()
report r@Result{survivors = m} = do
  putStr "Discarded: " >> print (improper r)
--  putStr "Killed: " >> print (killed r)
  putStrLn ""
  let xs                  =  toList (delete [] m) -- sortBy (comparing (snd . snd)) $
      rep (pids, (a,n,m))  
        | n > 0 = do
                print pids 
                putStr $ show n ++ " survivors"
                when (m > 0) $ putStr $ " and " ++ show m ++ " uncovered cases"
                putStrLn $ ". Minimal survivor:"
                print a >> putStrLn ""
                            
        | m > 0 = do 
                print pids
                putStrLn $ show m ++ " uncovered cases. Minimal uncovered:"
                print a >> putStrLn ""
  mapM_ rep xs


numbered :: [(a -> Outcome)] -> PropertySet a
numbered xs = zip [1..] xs


run :: (Eq b, Show b, Enumerable b, Parameter a) => 
   Int -> [((a -> b) -> Bool)] -> (a -> b) -> IO () 
run k prs f = do
  putStrLn $ "Killing "++show n++" mutants"
  putStrLn $ show n'++" proper mutants\n"
  report $ framework ms ps
  where
    (n,raws) = mutants k
    n' = length ms
    ms = tail $ filter mini $ raws

    -- ps 
    ps = numbered $ map (useMutant' f) prs 













-- Example 1
ex1 k n = run k [assoc n, single n, nil n] append


append = uncurry (++) :: ([Bool],[Bool]) -> [Bool]

assoc :: Int -> (([Bool],[Bool]) -> [Bool]) -> Bool
assoc k ap = let (+) = curry ap 
           in runTests k $ \(x,y,z) -> x + (y + z) == (x + y) + z

single :: Int -> (([Bool],[Bool]) -> [Bool]) -> Bool
single k ap = let (+) = curry ap 
           in runTests k $ \(x,xs) -> [x] + xs == x:xs

nil :: Int -> (([Bool],[Bool]) -> [Bool]) -> Bool
nil k ap = let (+) = curry ap 
           in runTests k $ \xs -> [] + xs == xs

