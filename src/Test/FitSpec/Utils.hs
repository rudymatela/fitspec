-- |
-- Module      : Test.FitSpec.Utils
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- General purpose utility functions for FitSpec
{-# LANGUAGE CPP #-}
module Test.FitSpec.Utils
  ( (...)
  , uncurry3
  , count
  , compositions
  , subsets
  , contained
  , contains
  , filterU
  , sortAndGroupOn
  , sortAndGroupFstBySnd
  , sortGroupAndCollapse
  , takeWhileIncreasing
  , takeWhileIncreasingOn
  , lastTimeout
  , sortOn
  , (***)
  )
where

#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#endif
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception ( Exception
                         , SomeException
                         , ArithException
                         , ArrayException
                         , ErrorCall
                         , PatternMatchFail
                         , catch
                         , catches
                         , Handler (Handler)
                         , evaluate
                         )
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (groupBy,sortBy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (liftM)

-- | Compose composed with compose operator.
--
-- > (f ... g) x y === f (g x y)
(...) :: (c->d) -> (a->b->c) -> a -> b -> d
(...) = (.) . (.)
-- f ... g = \x y -> f (g x y)

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- | 'compositions' @bs@ returns all compositions formed by taking values of @bs@
compositions :: [Bool] -> [Bool]
compositions = map and . subsets

-- | 'subsets' @xs@ returns the list of sublists formed by taking values of @xs@
subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

-- TODO: rename contained and contains to subset and superset?

-- | Check if all elements of a list is contained in another list
contained :: Eq a => [a] -> [a] -> Bool
xs `contained` ys = all (`elem` ys) xs

contains :: Eq a => [a] -> [a] -> Bool
contains = flip contained

-- | 'filterU' filter greater-later elements in a list according to a partial
--   ordering relation.
--
-- > filterU (notContained) [[1],[2],[1,2,3],[3,4,5]] == [[1],[2],[3,4,5]]
filterU :: (a -> a -> Bool) -> [a] -> [a]
filterU f []     = []
filterU f (x:xs) = x : filter (f x) (filterU f xs)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

sortAndGroupOn :: Ord b => (a -> b) -> [a] -> [[a]]
sortAndGroupOn f = groupBy ((==) `on` f)
                 . sortOn f

sortGroupAndCollapse :: Ord b
                     => (a -> b) -> (a -> c) -> (b -> [c] -> d)
                     -> [a] -> [d]
sortGroupAndCollapse f g h = map collapse
                           . sortAndGroupOn f
  where collapse (x:xs) = f x `h` map g (x:xs)

sortAndGroupFstBySnd :: Ord b => [(a,b)] -> [([a],b)]
sortAndGroupFstBySnd = sortGroupAndCollapse snd fst (flip (,))

-- | Takes values from a list while the values increase.  If the original list
--   is non-empty, the returning list will also be non-empty
takeWhileIncreasing :: (a -> a -> Ordering) -> [a] -> [a]
takeWhileIncreasing _ [] = []
takeWhileIncreasing _ [x] = [x]
takeWhileIncreasing cmp (x:y:xs) = x : case x `cmp` y of
                                         LT -> takeWhileIncreasing cmp (y:xs)
                                         _  -> []


takeWhileIncreasingOn :: Ord b => (a -> b) -> [a] -> [a]
takeWhileIncreasingOn f = takeWhileIncreasing (compare `on` f)

readIORefUntil :: (a -> Bool) -> IORef a -> IO a
readIORefUntil p r = do
  x <- readIORef r
  if p x
    then return x
    else threadDelay 100000 -- 100ms
      >> readIORefUntil p r

-- | @lastTimeout s xs@ will take the last value of @xs@ it is able evaluate
--   before @s@ seconds elapse.
lastTimeout :: Int -> [a] -> IO a
lastTimeout _ []     = error "lastTimeout: empty list"
lastTimeout 0 (x:_)  = return x  -- no time to lose
lastTimeout s xs = do
  r <- newIORef (undefined,False)
  tid <- forkIO $ keepImproving r xs
  threadDelay (s*1000000) -- TODO: change to waitForThread!!!
  (x,_) <- readIORefUntil snd r
  killThread tid
  return x
  where keepImproving _ []     = return ()
        keepImproving r (x:xs) = do
          evaluate x
          writeIORef r (x,True)
          keepImproving r xs

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f *** g = \(x,y) -> (f x, g y)
