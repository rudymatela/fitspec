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
             -> Listate (a,Heap a) (Heap a) (Listate (Heap a) (Heap a) (Listate (Heap a,Heap a) (Heap a) Bool))
nullProperty x = (return . return . return) True


insertCommut :: (Ord a, Listable a, Sized a)
             => (a,a,Heap a)
             -> Listate (a,Heap a) (Heap a) (Listate (Heap a) (Heap a) (Listate (Heap a,Heap a) (Heap a) Bool))
insertCommut (x,y,h) = do insertyh        <- lsMutateApply (y,h)
                          insertxinsertyh <- lsMutateApply (x,insertyh)
                          insertxh        <- lsMutateApply (x,h)
                          insertyinsertxh <- lsMutateApply (y,insertxh)
                          return . return . return $ insertyinsertxh == insertxinsertyh


propertyMap :: (Ord a, Sized a, Listable a)
            => Int
            -> ((a,Heap a) -> Heap a)
            -> (Heap a -> Heap a)
            -> ((Heap a,Heap a) -> Heap a)
            -> [[ ([Bool], Memo (a,Heap a) (Heap a), Memo (Heap a) (Heap a), Memo (Heap a,Heap a) (Heap a)) ]]
propertyMap n insert'' deleteMin' merge'' = runListate3 insert'' deleteMin' merge''
                                          $ getCompose . getCompose
                                          $ traverse (Compose . Compose)
  [ lholds3 n $ nullProperty 
  , lholds3 n $ insertCommut
  , lholds3 n $ \(h,x) -> do insertxh <- lapp x h
                             return . return . return $ null (insertxh) == False
  , lholds3 n $ \(h,h1,x) -> do insertxh1 <- lapp x h1
                                return . return $ do mergehinsertxh1 <- lapp h insertxh1
                                                     return True  -- 11  incomplete
  , lholds3 n $ \h -> return $ do deletemin_h <- lsMutateApply h
                                  return $ do merge_h_deletemin_h <- lapp h deletemin_h
                                              merge_h_h <- lapp h h
                                              return True -- 13 incomplete
  ]
  where lapp = uncurry lsMutateApply
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

