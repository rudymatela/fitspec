module FitSpec
  ( Memo (..)
  , initMemo
  , lookup
  , insert
  , toList

  , Listate (..)
  , Listate2 (..)
  , Listate3 (..)
  , runListate
  , runListate2
  , runListate3

  , lsMutateApply

  , lsMutateApply12
  , lsMutateApply22

  , lsMutateApply13
  , lsMutateApply23
  , lsMutateApply33

  , Sized (..)

  , Args (..)
  , args

  , lholds

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
import Utils


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

data Listate3 a b c d e f g = Listate3 { listates3 :: Memo a b
                                                   -> Memo c d
                                                   -> Memo e f
                                                   -> [[ (g,Memo a b,Memo c d,Memo e f) ]] }

runListate :: (a->b)
           -> Listate a b c
           -> [[ (c,Memo a b) ]]
runListate f = (`listates` initMemo f)


runListate2 :: (a->b)
            -> (c->d)
            -> Listate2 a b c d e
            -> [[ (e,Memo a b,Memo c d) ]]
runListate2 f g = \ls -> listates2 ls (initMemo f) (initMemo g)


runListate3 :: (a->b)
            -> (c->d)
            -> (e->f)
            -> Listate3 a b c d e f g
            -> [[ (g,Memo a b,Memo c d,Memo e f) ]]
runListate3 f g h = \ls -> listates3 ls (initMemo f) (initMemo g) (initMemo h)


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


lsMutateApply13 :: (Ord a, Eq b, Sized a, Listable b)
                => a
                -> Listate3 a b c d e f b
lsMutateApply13 x = Listate3 $ \m n o -> lsMutateApply' (\x m' -> (x,m',n,o)) x m

lsMutateApply23 :: (Ord c, Eq d, Sized c, Listable d)
                => c
                -> Listate3 a b c d e f d
lsMutateApply23 x = Listate3 $ \m n o -> lsMutateApply' (\x n' -> (x,m,n',o)) x n

lsMutateApply33 :: (Ord e, Eq f, Sized e, Listable f)
                => e
                -> Listate3 a b c d e f f
lsMutateApply33 x = Listate3 $ \m n o -> lsMutateApply' (\x o' -> (x,m,n,o')) x o


instance Functor (Listate a b) where
  fmap = liftM

instance Applicative (Listate a b) where
  pure = return
  (<*>) = ap

instance Monad (Listate a b) where
  return x = Listate $ \m -> [[ (x,m) ]]
  ff >>= gg = Listate $ \m -> lsConcatMap (\(x,m') -> listates (gg x) m') $ listates ff m

instance Functor (Listate2 a b c d) where
  fmap = liftM

instance Applicative (Listate2 a b c d) where
  pure = return
  (<*>) = ap

instance Monad (Listate2 a b c d) where
  return x = Listate2 $ \m m' -> [[ (x,m,m') ]]
  ff >>= gg = Listate2 $ \m m' -> lsConcatMap (\(x,m'',m''') -> listates2 (gg x) m'' m''') $ listates2 ff m m'

instance Functor (Listate3 a b c d e f) where
  fmap = liftM

instance Applicative (Listate3 a b c d e f) where
  pure = return
  (<*>) = ap

instance Monad (Listate3 a b c d e f) where
  return x = Listate3 $ \m m' m'' -> [[ (x,m,m',m'') ]]
  ff >>= gg = Listate3
            $ \m m' m'' -> lsConcatMap (\(x,m''',m'''',m''''') -> listates3 (gg x) m''' m'''' m''''')
                                     $ listates3 ff m m' m''

ltest :: Monad m
      => (c -> m Bool)
      -> [c]
      -> m Bool
ltest prop xs = foldr (liftM2 (&&)) (return True) (map prop xs)

lholds :: (Monad m, Listable c)
       => Int
       -> (c -> m Bool)
       -> m Bool
lholds n prop = ltest prop (take n list)


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
                       $ fitspec fst snd nf rss
  where showResult (x,y,mm) = [ show x, show y, showM mm ]
        showM Nothing  = ""
        showM (Just m) = showAsCaseExp (functionName args) (variableName args) m

fitspec :: (a -> [Bool]) -> (a -> b)
        -> Int -> [[a]] -> [(Int, [Int], Maybe b)]
fitspec getrs getsm n rss@((p:_):_) =
      filterU relevant
    . sortBy (comparing $ \(s,ids,_) -> (s, length ids))
    . tag
    . fitspec' getrs getsm n
    $ rss
  where tag = zipWith (\ids (ns,sms) -> (ns,ids,sms)) (subsets [1..length (getrs p)])
        (n,is,_) `relevant` (n',is',_) = not (n == n' && is `contained` is')
fitspec _ _ _ _ = error "fitspec: something went wrong, no results to analyse?"

fitspec' :: (a -> [Bool]) -> (a -> b)
         -> Int -> [[a]] -> [(Int, Maybe b)]
fitspec' getrs getsm n =
      map (\fs -> (count isJust fs, listToMaybe $ catMaybes fs))
    . transpose
    . map (\p -> map (\b -> if b then Just (getsm p) else Nothing) (compositions (getrs p)))
    . take n . tail . concat
  where count f = length . filter f


-- | 'compositions' @bs@ returns all compositions formed by taking values of @bs@
compositions :: [Bool] -> [Bool]
compositions = map and . subsets

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
