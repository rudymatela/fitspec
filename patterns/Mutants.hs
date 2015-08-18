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


mutants :: Mutable a => Int -> (Integer, [Mutates a])
mutants k = (crds, vs) where
  crds = sum $ take (k+1) $ count $ countparam vs
  vs = concat $ values' k

runTests :: Enumerable x => Int -> (x -> Bool) -> Bool
runTests n p = all p (take n (concat allValues))


data Outcome = Survived
             | Killed
             | Unmatched
             | Unchanged
             deriving (Eq, Ord ,Show)

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
  go n []      r  = put n >> putStrLn "" >> return r
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

data Mutated a = NotMutated | Mutated a deriving (Typeable, Eq)
instance Show a => Show (Mutated a) where
  show NotMutated   = "original i"
  show (Mutated a)  = show a
instance Enumerable a => Enumerable (Mutated a) where
  enumerate = share $ c0 NotMutated <|> c1 Mutated

toMutated :: Maybe a -> Mutated a 
toMutated = maybe NotMutated Mutated
mutToMaybe :: Mutated a -> Maybe a
mutToMaybe NotMutated = Nothing
mutToMaybe (Mutated a) = Just a


showMutant :: Mutates a -> String
showMutant (F f)      = ("\\i@" ++) . drop 1 . show $ f
showMutant (T (a,b))  = unlines [showMutant a,"--",showMutant b]
showMutant (O (OM f))      = 
  "Shallow mutant: " 
  ++ show (take 5 [(i,fi) | i <- concat $ values' 100
                          , let fi = f i
                          -- , i /= fi -- Does not work for some reason
                          ])

{-
run2 :: (Output c, Parameter a, Parameter b) => 
   Int -> [((a -> b -> c) -> Bool)] -> (a -> b -> c) -> IO () 
run2 k prs f = runM k (map (. curry) prs) (uncurry f)
-}

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
  F :: (Eq b, Show b) => (Function a (Mutated b)) -> Mutates (a -> b)
  T :: (Mutates a, Mutates b) -> Mutates (a,b)
  
  O :: (Enumerable b, Eq b, Show b) => OM b -> Mutates (a -> b)
  
  -- For automatic uncurrying
  -- U :: Mutates ((a,b) -> c) -> Mutates (a -> b -> c)

isMinimalM :: Mutates a -> Bool
isMinimalM (F f)      = isMinimal f
isMinimalM (T (a,b))  = isMinimalM a && isMinimalM b
isMinimalM (O{})      = True


-- deriving instance Show (Mutates a)


class Typeable a => Mutable a where
  mutates :: (Typeable f, Sized f) => Shareable f (Mutates a)
  
  -- mutFun :: (Typeable f, Sized f, Parameter x) => Shareable f (Mutates (x -> a))
  -- mutFun = fmap F access
  
instance (Parameter a, Output b) => Mutable (a -> b) where
  mutates = fmap F access <|> fmap O access

instance (Mutable a, Mutable b) => Mutable (a,b) where
  mutates = fmap T access

instance Mutable a => Enumerable (Mutates a) where
  enumerate = share mutates




class (Eq a, Show a, Enumerable a) => Output a where
  mutateO :: (Typeable f, Sized f) => Shared f (OM a)
  mutateO = share empty
  
  genesisO :: (Typeable f, Sized f) => Shared f (OG a)
  genesisO = share empty

-- Output mutators
newtype OG a = OG {og :: a}

newtype OM a = OM {om :: a -> a}
mkOM :: (Typeable f, Sized f, Typeable a) => [Shareable f (OM a)] -> Shared f (OM a)
mkOM = share . aconcat

om0 :: (Sized f, Typeable f) => (b -> b) -> Shareable f (OM b)
om0 = pure . OM

om1 :: (Sized f, Typeable f, Output a) => ((a -> a) -> (b -> b)) -> Shareable f (OM b)
om1 f = fmap (OM . f . om) access

-- om2 :: (Sized f, Typeable f, Output a, Output b) => ((a -> a) -> (b -> b) -> (c -> c)) -> Shareable f (OM c)
-- om2 f = om1 (uncurry f)

instance Output a => Enumerable (OM a) where
  enumerate = mutateO

instance Output a => Enumerable (OG a) where
  enumerate = genesisO


instance Output () where
  genesisO = share $ pure $ OG ()

-- Mutate either left or right values
instance (Output a, Output b) => Output (Either a b) where
  mutateO = share $ fmap OM $ c1 ml <|> c1 mr -- c1 mboth <|> 
    where
      mboth (OM f,OM g) = either (Left . f) (Right . g)
      ml (OM f) = either (Left . f) Right
      mr (OM f) = either Left (Right . f)

  genesisO = share $ fmap OG $ (c1 (Left . og)) <|> (c1 (Right . og))

-- Mutate either first or second value
instance (Output a, Output b) => Output (a,b) where
  mutateO = mkOM [om1 mfst, om1 msnd] where
    mfst f (a,b) = (f a, b)
    msnd f (a,b) = (a, f b)

  genesisO = share $ fmap OG $ c1 $ \(OG a, OG b) -> (a,b)


instance Output Bool where 
  mutateO = mkOM [om0 not]
  genesisO = share $ fmap OG $ pure False <|> pure True

instance Output a => Output [a] where
  mutateO = mkOM [ om1 mall
                 , fmap OM $ c1 mcons
                 , om0 reverse
                 ] where
    mfst f (x:xs)  = f x : xs
    mfst _ xs      = xs
    
    mall f xs      = map f xs
    
    mcons :: OG a -> [a] -> [a]
    mcons (OG a) = (a:)

  genesisO = share $ fmap OG $ pure [] <|> c1 (:[])

instance Output a => Output (Maybe a) where
  mutateO = mkOM [om1 mjust, fmap OM $ c1 mnothing] where 
    mjust f = fmap f
    mnothing x = maybe (Just x) Just

instance Output Int where
  mutateO = mkOM [om0 (+1), om0 (`subtract` 1)]
  genesisO = share $ fmap OG $ pure 0 <|> pure maxBound <|> pure minBound



mutateM :: a -> Mutates a -> IO (IO (Bool,Bool), a)
mutateM f (F m)            = attach f (m $$)
mutateM f (O (OM g))       = attachO f g
mutateM p (T (ma,mb))      = case p of
  (a,b) -> do
    (ioa, ra) <- mutateM a ma
    (iob, rb) <- mutateM b mb
    let merge = do
         (b1,b2) <- ioa
         (b3,b4) <- iob
         return (b1 || b3, b2 || b4)
    return (merge,(ra,rb))

attach :: Eq b => (a -> b) -> (a -> Mutated b) -> IO (IO (Bool,Bool), a -> b)
attach original mutant = do
  r <- newIORef False -- Has the function been executed on a modified value
  s <- newIORef False -- Has the function been executed
  let f a = maybe (original a) (x a) (mutToMaybe $ mutant a)
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


attachO :: Eq b => (a -> b) -> (b -> b) -> IO (IO (Bool,Bool), a -> b)
attachO original mutant = do
  r <- newIORef False -- Has the function been executed on a modified value
  let f a = unsafePerformIO $ do
                let o = original a
                    m = mutant o
                when (m /= o) (writeIORef r True)
                return m
      m = do
        x <- readIORef r
        return $ if x then (True,True) else (False, False)
  return (m, f)

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
validateV vf a ma | anyo ma = return True where
  anyo :: Mutates a -> Bool
  anyo F{}          = False
  anyo O{}          = True
  anyo (T (ma,mb))  = anyo ma || anyo mb
validateV vf a ma = catch (vf (a `mutateV` ma ) `seq` return False ) (\Stop -> return True)
  where
    mutateV :: a -> Mutates a -> a
    mutateV f (F m)        = attachV f (m $$)
    mutateV p (T (ma,mb))  = case p of
      (a,b) -> (mutateV a ma, mutateV b mb)
      
    attachV :: Eq b => (a -> b) -> (a -> Mutated b) -> (a -> b)
    attachV original mutant = f where
      f a = maybe (original a) (x a) (mutToMaybe $ mutant a) 
      x a b = if (b /= original a) then throw Stop else b





