-- | General purpose utility functions.
module Utils
  ( uncurry3
  , boolToMaybe
  , eithers
  , subsets
  , contained
  , spread
  , filterU
  , indexDefault
  , bindArgumentType
  , errorToNothing
  , errorToFalse
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

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

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

-- | Check if all elements of a list is contained in another list
contained :: Eq a => [a] -> [a] -> Bool
xs `contained` ys = all (`elem` ys) xs

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
  (Just <$> evaluate x) `catch` \e -> do let _ = e :: SomeException
                                         return Nothing

-- | Transforms a value into 'Just' that value or 'Nothing' on some errors:
--
--   * ArithException
--   * ArrayException
--   * ErrorCall
--   * PatternMatchFail
errorToNothing :: a -> Maybe a
errorToNothing x = unsafePerformIO $
  (Just <$> evaluate x) `catches` map ($ return Nothing)
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
