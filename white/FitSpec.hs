module FitSpec
  ( Memo (..)
  , initMemo
  , lookup
  , insert
  , toList

  , Listate (..)
  , runListate
  , lsMutateApply

  , Sized (..)

  , Args (..)
  , args

  , lholds
  , lholds2
  , lholds3

  , report
  , reportWith
  , fitspec
  )
where

import Test.Check
import Control.Monad
import Data.List (transpose,sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing, catMaybes, listToMaybe)
import Table
import qualified Data.Map as Map (insert,lookup,empty,Map,toList)
import Prelude hiding (lookup)
import Control.Applicative (liftA, liftA2)
import Data.Functor.Compose


-- In the end, this can be merged with Listable.  It can be even generated by
-- the same combinators: in a similar way to what Uniplate does when generating
-- its different traversing functions
class Sized a where
  size :: a -> Int

instance Sized Int where
  size = id

instance Sized Bool where
  size _ = 0

instance (Sized a) => Sized [a] where
  size = sum . map ((+1) . size)

instance (Sized a, Sized b) => Sized (a,b) where
  size (x,y) = size x + size y


data Memo a b = Memo (a->b) (Map.Map a (Maybe b))

initMemo :: (a->b) -> (Memo a b)
initMemo f = Memo f Map.empty

lookup :: Ord a => a -> Memo a b -> Maybe (Maybe b)
lookup x (Memo f bs) = Map.lookup x bs

insert :: Ord a => a -> Maybe b -> Memo a b -> Memo a b
insert x y (Memo f bs) = Memo f (Map.insert x y bs)

toList :: Memo a b -> [(a,b)]
toList (Memo _ bs) = map (\(x,Just y) -> (x,y))
                   . filter (\(_,m) -> isJust m)
                   $ Map.toList bs

instance (Eq b, Show a, Show b) => Show (Memo a b) where
  show m = ("Memo" ++) . show $ toList m

data Listate a b c = Listate { listates :: Memo a b
                                        -> [[ (c,Memo a b) ]] -- a listing
                             }

data Listate2 a b c d e = Listate2 { listates2 :: Memo a b
                                               -> Memo c d
                                               -> [[ (e,Memo a b,Memo c d) ]] }

runListate :: (a->b)
           -> Listate a b c
           -> [[ (c,Memo a b) ]]
runListate f = (`listates` initMemo f)


lsMutateApply :: (Ord a, Eq b, Sized a, Listable b)
              => a
              -> Listate a b b
lsMutateApply x = Listate $ lsMutateApply' (,) x


lsMutateApply' :: (Eq b, Ord a, Listable b, Sized a)
               => (b -> Memo a b -> t)
               -> a
               -> Memo a b
               -> [[t]]
lsMutateApply' tup x (m@(Memo f bs)) =
  case lookup x m of
    Nothing  -> [f x `tup` insert x Nothing m]
              : (replicate (size x) [] ++
                 lsmap (\y -> y `tup` insert x (Just y) m) (lsfilter (/= f x) listing))
    (Just Nothing)  -> [[ f x `tup` m ]]
    (Just (Just y)) -> [[ y `tup` m ]]


lsMutateApply12 :: (Ord a, Eq b, Sized a, Listable b)
                => a
                -> Listate2 a b c d b
lsMutateApply12 x = Listate2 $ \m m' -> lsMutateApply' (\x m'' -> (x,m'',m')) x m

lsMutateApply22 :: (Ord c, Eq d, Sized c, Listable d)
                => c
                -> Listate2 a b c d d
lsMutateApply22 x = Listate2 $ \m' m -> lsMutateApply' (\x m'' -> (x,m',m'')) x m


instance Functor (Listate a b) where
  fmap = liftM

instance Applicative (Listate a b) where
  pure = return
  (<*>) = ap

instance Monad (Listate a b) where
  return x = Listate $ \m -> [[ (x,m) ]]
  ff >>= gg = Listate $ \m -> lsConcatMap (\(x,m') -> listates (gg x) m') $ listates ff m


ltest :: (c -> Listate a b Bool)
      -> [c]
      -> Listate a b Bool
ltest prop xs = foldr (liftM2 (&&)) (return True) (map prop xs)

lholds :: Listable c
       => Int
       -> (c -> Listate a b Bool)
       -> Listate a b Bool
lholds n prop = ltest prop (take n list)

ltest2 :: (e -> Listate a b (Listate c d Bool))
       -> [e]
       -> Listate a b (Listate c d Bool)
ltest2 prop xs = getCompose
               $ foldr (liftA2 (&&)) (pure True) (map (fmap Compose prop) xs)

lholds2 :: Listable e
        => Int
        -> (e -> Listate a b (Listate c d Bool))
        -> Listate a b (Listate c d Bool)
lholds2 n prop = ltest2 prop (take n list)

ltest3 :: (g -> Listate a b (Listate c d (Listate e f Bool)))
       -> [g]
       -> Listate a b (Listate c d (Listate e f Bool))
ltest3 prop xs = getCompose . getCompose
               $ foldr (liftA2 (&&)) (pure True) (map (fmap (Compose . Compose) prop) xs)

lholds3 :: Listable g
        => Int
        -> (g -> Listate a b (Listate c d (Listate e f Bool)))
        -> Listate a b (Listate c d (Listate e f Bool))
lholds3 n prop = ltest3 prop (take n list)

-- | Extra arguments for reporting functions
data Args = Args
          { functionName :: String
          , variableName :: String
          , limitResults :: Maybe Int -- maximum number of results
          }

-- | Default arguments
args = Args { functionName = "function"
            , variableName = "x"
            , limitResults = Nothing    -- show everything
            }

report :: (Show a, Show b, Eq b) => Int -> [[([Bool], Memo a b)]] -> IO ()
report nf rss = reportWith args nf rss

reportWith :: (Show a, Show b, Eq b) => Args -> Int -> [[([Bool], Memo a b)]] -> IO ()
reportWith args nf rss = putStrLn
                       . table "   "
                       . map showResult
                       . maybe id take (limitResults args)
                       $ fitspec nf rss
  where showResult (x,y,mm) = [ show x, show y, showM mm ]
        showM Nothing  = ""
        showM (Just m) = showAsCaseExp (functionName args) (variableName args) m

fitspec :: Int -> [[([Bool], a)]] -> [(Int, [Int], Maybe a)]
fitspec n rss@(((rs,_):_):_) = filterU relevant
                             . sortBy (comparing $ \(s,ids,_) -> (s, length ids))
                             . tag
                             . fitspec' n
                             $ rss
  where tag = zipWith (\ids (rs,f) -> (rs,ids,f)) (subsets [1..length rs])
        (n,is,_) `relevant` (n',is',_) = not (n == n' && is `contained` is')
fitspec _ _ = error "fitspec: something went wrong, no results to analyse?"

fitspec' :: Int -> [[([Bool], a)]] -> [(Int, Maybe a)]
fitspec' n = map (\fs -> (count isJust fs, listToMaybe $ catMaybes fs))
           . transpose
           . map (\(rs,f) -> map (\b -> if b then Just f else Nothing) (compositions rs))
           . take n . tail . concat
  where count f = length . filter f


-- | 'subsets' @xs@ returns the list of sublists formed by taking values of @xs@
--
-- The return list starts with the complete set
subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

-- | 'compositions' @bs@ returns all compositions formed by taking values of @bs@
compositions :: [Bool] -> [Bool]
compositions = map and . subsets

-- | Check if all elements of a list is contained in another list
contained :: Eq a => [a] -> [a] -> Bool
xs `contained` ys = all (`elem` ys) xs

-- | 'filterU' filter greater-later elements in a list according to a partial
--   ordering relation.
--
-- > filterU (notContained) [[1],[2],[1,2,3],[3,4,5]] == [[1],[2],[3,4,5]]
filterU :: (a -> a -> Bool) -> [a] -> [a]
filterU f []     = []
filterU f (x:xs) = x : filter (f x) (filterU f xs)

showAsCaseExp :: (Show a, Show b, Eq b)
              => String -> String -> Memo a b -> String
showAsCaseExp funName varName m =
  case bindings of
    [] -> "\\" ++ varName ++ " -> " ++ funName ++ " " ++ varName ++ "\n"
    _  -> columns " " [ "\\" ++ varName ++ " -> case " ++ varName ++ " of ", caseExp ] ++ "\n"
  where caseExp = table " -> "
                . (++ [["_", funName ++ " " ++ varName]])
                . map (\(a,r) -> [show a, show r])
                $ bindings
        bindings = toList m
