-- | General purpose utility functions.
module Utils
  ( (...)
  , uncurry3
  , uncurry4
  , count
  , compositions
  , boolToMaybe
  , eithers
  , subsets
  , contained
  , contains
  , spread
  , filterU
  , indexDefault
  , bindArgumentType
  , errorToNothing
  , errorToFalse
  , sortAndGroupOn
  , takeWhileIncreasing
  , takeWhileIncreasingOn
  , lastTimeout
  , sortOn
  , (***)
  )
where

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
import Data.IORef (newIORef, readIORef, writeIORef)
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

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (x,y,z,w) = f x y z w

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- | 'compositions' @bs@ returns all compositions formed by taking values of @bs@
compositions :: [Bool] -> [Bool]
compositions = map and . subsets

-- | Given a value and a boolean, returns Just if True.
boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe x p = if p
                    then Just x
                    else Nothing

-- | 'eithers' returns all lefts and rights (of the same types) in a list
--
-- > eithers [Left x, Right y, Left z, Right w] == [x,y,z,w]
eithers :: [Either a a] -> [a]
eithers = map (either id id)

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

-- | Spread applies a function to different elements of a list to build
--   different lists.
--
-- > spread f [x,y,z]  ==  [ [f x, y, z], [x, f y, z], [x, y, f z] ]
spread :: (a -> a) -> [a] -> [[a]]
spread f [] = []
spread f (x:xs) = (f x:xs) : map (x:) (spread f xs)

-- | 'filterU' filter greater-later elements in a list according to a partial
--   ordering relation.
--
-- > filterU (notContained) [[1],[2],[1,2,3],[3,4,5]] == [[1],[2],[3,4,5]]
filterU :: (a -> a -> Bool) -> [a] -> [a]
filterU f []     = []
filterU f (x:xs) = x : filter (f x) (filterU f xs)

indexDefault :: [a] -> Int -> a -> a
indexDefault []     _ x = x
indexDefault (x:_)  0 _ = x
indexDefault (_:xs) n x = indexDefault xs (n-1) x

-- | Takes a value and a function.  Ignores the value.  Binds the argument of
--   the function to the type of the value.
bindArgumentType :: a -> (a -> b) -> a -> b
bindArgumentType _ f = f

-- | Transforms a value into 'Just' that value or 'Nothing' on error.
anyErrorToNothing :: a -> Maybe a
anyErrorToNothing x = unsafePerformIO $
  (Just `liftM` evaluate x) `catch` \e -> do let _ = e :: SomeException
                                             return Nothing

-- | Transforms a value into 'Just' that value or 'Nothing' on some errors:
--
--   * ArithException
--   * ArrayException
--   * ErrorCall
--   * PatternMatchFail
errorToNothing :: a -> Maybe a
errorToNothing x = unsafePerformIO $
  (Just `liftM` evaluate x) `catches` map ($ return Nothing)
                                      [ hf (undefined :: ArithException)
                                      , hf (undefined :: ArrayException)
                                      , hf (undefined :: ErrorCall)
                                      , hf (undefined :: PatternMatchFail)
                                      ]
  where hf :: Exception e => e -> IO a -> Handler a -- handlerFor
        hf e h = Handler $ bindArgumentType e (\_ -> h)

errorToFalse :: Bool -> Bool
errorToFalse p = case errorToNothing p of
                   Just p' -> p
                   Nothing -> False

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

sortAndGroupOn :: Ord b => (a -> b) -> [a] -> [[a]]
sortAndGroupOn f = groupBy ((==) `on` f)
                 . sortOn f


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


-- | @lastTimeout s xs@ will take the last value of @xs@ it is able evaluate
--   before @s@ seconds elapse.
lastTimeout :: Int -> [a] -> IO a
lastTimeout _ []     = error "lastTimeout: empty list"
lastTimeout 0 (x:_)  = return x  -- no time to lose
lastTimeout s (x:xs) = do
  r <- newIORef x
  tid <- forkIO $ keepImproving r xs
  threadDelay (s*1000000) -- TODO: change to waitForThread!!!
  killThread tid
  readIORef r
  where keepImproving _ []     = return ()
        keepImproving r (x:xs) = do
          evaluate x
          writeIORef r x
          keepImproving r xs

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f *** g = \(x,y) -> (f x, g y)
