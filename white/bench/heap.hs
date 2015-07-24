-- TODO: use runListate3 and lholds3 (to be implemented...)

import FitSpec hiding (insert,toList)
import Test.Check
import Test.Check.Debug
import Control.Monad
import Data.Functor.Compose
import Prelude hiding (null,toList)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Heap

{-
headIsHead :: (Ord a, Listable a, Sized a)
           => (a,[a])
           -> Listate [a] [a] (Listate [a] a Bool)
headIsHead (x,xs) = return $ do headxxs <- lsMutateApply (x:xs)
                                return $ x == headxxs

tailIsTail :: (Ord a, Listable a, Sized a)
           => (a,[a])
           -> Listate [a] [a] (Listate [a] a Bool)
tailIsTail (x,xs) = do tailxxs <- lsMutateApply (x:xs)
                       return . return $ xs == tailxxs
-}

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
infixr 0 ==>

instance (Ord a, Listable a) => Listable (Heap a) where
  listing = cons1 fromList

instance (Ord a, Sized a) => Sized (Heap a) where
  size = size . toList


nullProperty :: (Ord a, Listable a, Sized a)
             => a
             -> Listate3 (a,Heap a) (Heap a)  (Heap a) (Heap a)  (Heap a,Heap a) (Heap a) Bool
nullProperty x = return True


insertCommut :: (Ord a, Listable a, Sized a)
             => (a,a,Heap a)
             -> Listate3 (a,Heap a) (Heap a)  (Heap a) (Heap a)  (Heap a,Heap a) (Heap a) Bool
insertCommut (x,y,h) = do insertyh        <- insert' y h
                          insertxinsertyh <- insert' x insertyh
                          insertxh        <- insert' x h
                          insertyinsertxh <- insert' y insertxh
                          return $ insertyinsertxh == insertxinsertyh
  where insert' = curry lsMutateApply13


propertyMap :: (Ord a, Sized a, Listable a)
            => Int
            -> ((a,Heap a) -> Heap a)
            -> (Heap a -> Heap a)
            -> ((Heap a,Heap a) -> Heap a)
            -> [[ ([Bool], Memo (a,Heap a) (Heap a), Memo (Heap a) (Heap a), Memo (Heap a,Heap a) (Heap a)) ]]
propertyMap n insert'' deleteMin'' merge'' = runListate3 insert'' deleteMin'' merge''
                                          $ sequence
  [ lholds3 n $ nullProperty 
  , lholds3 n $ insertCommut
  , lholds3 n $ \(h,x) -> do insertxh <- insert' x h
                             return $ null (insertxh) == False
  , lholds3 n $ \(h,x) -> do insertxh <- insert' x h
                             return $ L.insert x (toList h) == toList (insertxh)
  ,                       do deleteMinNil <- deleteMin' Nil
                             return $ deleteMinNil == Nil
  , lholds3 n $ \(h,h1) -> do mergehh1 <- merge' h h1
                              mergeh1h <- merge' h1 h
                              return $ mergehh1 == mergeh1h
  , lholds3 n $ \h -> do mergehnil <- merge' h Nil
                         return $ mergehnil == h
  , lholds3 n $ \(h,h1,h2) -> do mergeh1h2 <- merge' h1 h2
                                 mergehh1h2 <- merge' h mergeh1h2
                                 mergehh2 <- merge' h h2
                                 mergeh1hh2 <- merge' h1 mergehh2
                                 return $ mergehh1h2 == mergeh1hh2
  ]
  where insert' = curry lsMutateApply13
        deleteMin' = lsMutateApply23
        merge' = curry lsMutateApply33
  {-
  [ lholds3 n $ \x y h ->      insert' x (insert' y h) == insert' y (insert' x h) --  1
  , lholds3 n $ \h x ->             null (insert' x h) == False                   --  2
  , lholds3 n $ \x h ->          L.insert x (toList h) == toList (insert' x h)    --  3

  ,                                   deleteMin' Nil == Nil                       --  4

  , lholds3 n $ \h h1 ->                   merge' h h1 == merge' h1 h             --  5
  , lholds3 n $ \h ->                     merge' h Nil == h                       --  6
  , lholds3 n $ \h h1 h2 ->    merge' h (merge' h1 h2) == merge' h1 (merge' h h2) --  7
  , lholds3 n $ \h ->             findMin (merge' h h) == findMin h               --  8
  , lholds3 n $ \h ->                null (merge' h h) == null h                  --  9
  , lholds3 n $ \h h1 ->           (null h && null h1) == null (merge' h h1)      -- 10

  , lholds3 n $ \h h1 x ->     merge' h (insert' x h1) == insert' x (merge' h h1) -- 11
  , lholds3 n $ \h ->          merge' h (deleteMin' h) == deleteMin' (merge' h h) -- 12
  , lholds3 n $ \x ->       deleteMin' (insert' x Nil) == Nil                     -- 13
  ]
  -}


main :: IO ()
main = do putLL 4 $ propertyMap 4 (uncurry insert) (deleteMin :: Heap Bool -> Heap Bool) (uncurry merge)

