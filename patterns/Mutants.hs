{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Mutants 
  ( module Mutants
  , module Control.Enumerable
  , module Control.Enumerable.Functions
  , module Control.Enumerable.Values
  ) where
import Control.Enumerable
import Control.Enumerable.Functions
import Control.Enumerable.Values
import Control.Enumerable.Count

import System.IO.Unsafe
import Data.IORef
import Control.Monad(when, filterM)
import Data.Maybe(isJust, isNothing)
-- import Data.Either(partitionEithers)

import Control.Exception

import Data.List (sortBy, sort)
import Data.Ord (comparing)

import System.IO(hFlush, hPutStr, stdout)

import qualified Data.Map as M (Map, insertWith, toList, empty, delete)

import BitMask

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

mutants :: Mutable a => Int -> (Integer, [Mutates a])
mutants k = (crds, vs) where
  crds = sum $ take (k+1) $ count $ countparam vs
  vs = concat $ values' k

mutate :: Eq b => (a -> b) -> Mutator a b -> IO (IO (Bool,Bool), (a -> b))
mutate f m = attach f (m $$)


data Outcome = Survived
             | Killed
             | Unmatched
             | Unchanged
             deriving (Eq, Ord ,Show)


-- useMutant' :: a -> (a -> Bool) -> Mutates a -> Outcome
-- useMutant' f p m = unsafePerformIO (useMutant f p m) -- This is probably safe, right?


runTests :: Enumerable x => Int -> (x -> Bool) -> Bool
runTests n p = all p (take n (concatMap values [0..]))

prepost :: Enumerable a => Int -> (a -> b) -> (a -> b -> Bool) -> Bool
prepost n f p = runTests n $ \a -> p a (f a)



type PropertyID     = Int
type PropertySet a  = [(PropertyID, a -> IO Outcome)]
data Result a = Result 
  { survivors :: M.Map BitSet (a,Int,Int) -- Maps property subsets to minimal survivor, and number of survivors divided into actual/uncovered survivors
  , improper  :: Int
--  , killed    :: Int
  }


result :: [(PropertyID,Outcome)] -> a -> Result a -> Result a
result ps a r
  | Survived `elem` os       = rep 1 0  -- Actual survivor
  | Killed `elem` os         = rep 0 1  -- Failure to match on subset -- What if os is all UCh?
  | Unchanged `elem` os      = r{improper = improper r + 1} -- Either kill or survived, later
  | otherwise                = rep 0 1  -- Failure to match on complete set
  where pids  = fromList [ pid | (pid,o) <- ps, o /= Killed]
        
        os                        = map snd ps
        merge (new,n,x) (old,m,y) = (if m == 0 && n > 0 then new else old,n+m,x+y)
        rep n x                   = r{survivors = M.insertWith merge pids (a,n,x) (survivors r)}

framework :: [a] -> PropertySet a -> IO (Result a)
framework ms ps = go 0 ms Result{survivors = M.empty, improper = 0} where
  go n []      r  = putStrLn "" >> return r
  go n (x:xs)  r  = do 
    put n
    ps <- sequence [fmap ((,) pid) (p x) | (pid,p) <- ps ]
    go (n+1) xs $ result ps x r
    
  put n = reStr $ "Done: "++show n
  reStr s = do 
    hPutStr stdout $ replicate (length s) '\b' ++ s
    hFlush stdout


report :: Int -> Result (Mutates a) -> IO ()
report numprops r@Result{survivors = m} = do
  putStr "Discarded: " >> print (improper r)
--  putStr "Killed: " >> print (killed r)
  putStrLn ""
  let keys                =  map fst $ M.toList m
      xs                  =  M.toList (M.delete emp m) -- sortBy (comparing (snd . snd)) $
      rep (pids, (a,n,m))  
        | n > 0 = do
                print (toList pids) 
                putStr $ show n ++ " survivors"
                when (m > 0) $ putStr $ " and " ++ show m ++ " uncovered cases"
                putStrLn $ ". Minimal survivor:"
                putStrLn (showMutant a)
                putStrLn ""
                            
        | m > 0 = do 
                print (toList pids) 
                putStrLn $ show m ++ " uncovered cases. Minimal uncovered:"
                putStrLn (showMutant a)
                putStrLn ""
  mapM_ rep xs

  putStrLn ""
  putStrLn "mimimal complete sets: "
  mapM_ (print . toList) (findComplete numprops keys)
  putStrLn ""

numbered :: [(a -> IO Outcome)] -> PropertySet a
numbered xs = zip [1..] xs

data Mutated a = NotMutated | Mutated a
instance Show a => Show (Mutated a) where
  show NotMutated   = "original i"
  show (Mutated a)  = show a
toMutated :: Maybe a -> Mutated a 
toMutated = maybe NotMutated Mutated

showMutant :: Mutates a -> String
showMutant (F f)     = ("\\i@" ++) . drop 1 . show . fmap toMutated $ f
showMutant (T (a,b))  = unlines [showMutant a,"--",showMutant b]



run2 :: (Eq c, Show c, Enumerable c, Parameter a, Parameter b) => 
   Int -> [((a -> b -> c) -> Bool)] -> (a -> b -> c) -> IO () 
run2 k prs f = runM k (map (. curry) prs) (uncurry f)


runM :: Mutable a => Int -> ([a -> Bool]) -> a -> IO ()
runM = runV Nothing

-- Run with a validating function that checks reachability for the mutants
runV :: Mutable a => Maybe (a -> ()) -> Int -> ([a -> Bool]) -> a -> IO ()
runV val k prs f = do
  putStrLn $ "Killing "++show n++" mutants"
  putStrLn $ show n'++" proper mutants\n"
  ms' <- case val of 
    Nothing -> return ms
    Just vf -> do 
      putStrLn "Validating mutants"
      xs <- filterM (validateV vf f) ms
      putStrLn $ show (length xs) ++ " mutants validated"
      return xs
      
  framework ms' ps >>= report (length prs)
  where
    (n,raws) = mutants k
    n' = length ms
    ms = tail $ filter isMinimalM $ raws
    ps = numbered $ map (useMutant f) prs 




data Mutates a where
  F :: (Eq b, Show b) => (Function a (Maybe b)) -> Mutates (a -> b)
  T :: (Mutates a, Mutates b) -> Mutates (a,b)

isMinimalM :: Mutates a -> Bool
isMinimalM (F f)      = isMinimal f
isMinimalM (T (a,b))  = isMinimalM a && isMinimalM b

deriving instance Show (Mutates a)


class Typeable a => Mutable a where
  mutates :: (Typeable f, Sized f) => Shareable f (Mutates a)
  
instance (Parameter a, Enumerable b, Eq b, Show b) => Mutable (a -> b) where
  mutates = fmap F access

instance (Mutable a, Mutable b) => Mutable (a,b) where
  mutates = fmap T access -- fmap T global


instance Mutable a => Enumerable (Mutates a) where
  enumerate = share mutates

mutateM :: a -> Mutates a -> IO (IO (Bool,Bool), a)
mutateM f (F m)            = attach f (m $$)
mutateM p (T (ma,mb))  = case p of
  (a,b) -> do
    (ioa, ra) <- mutateM a ma
    (iob, rb) <- mutateM b mb
    let merge = do
         (b1,b2) <- ioa
         (b3,b4) <- iob
         return (b1 || b3, b2 || b4)
    return (merge,(ra,rb))


useMutant :: a -> (a -> Bool) -> Mutates a -> IO Outcome
useMutant f p m = do
  (mb,f') <- mutateM f m
  case p f' of 
    True   -> mb >>= \(proper,used) -> case (proper,used) of
                      (False,False)    -> return Unmatched
                      (True,True)      -> return Survived
                      (False,True)     -> return Unchanged
    False  -> return Killed


data Stop = Stop deriving (Show, Typeable)
instance Exception Stop

validateV :: (a -> ()) -> a -> Mutates a -> IO Bool
validateV vf a ma = catch (vf (a `mutateV` ma ) `seq` return False ) (\Stop -> return True)

mutateV :: a -> Mutates a -> a
mutateV f (F m)            = attachV f (m $$)
mutateV p (T (ma,mb))  = case p of
  (a,b) -> (mutateV a ma, mutateV b mb)

attachV :: Eq b => (a -> b) -> (a -> Maybe b) -> (a -> b)
attachV original mutant = f where
  f a = maybe (original a) (x a) (mutant a) 
  x a b = if (b /= original a) then throw Stop else b


