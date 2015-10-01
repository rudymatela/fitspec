{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec hiding (insert,toList)
import Test.Check
import Control.Monad
import Data.Functor.Compose
import Prelude hiding (null,toList)
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import Heap
import Utils (errorToFalse)


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
  [ lholds n $ nullProperty                                                     -- 0 (just a test)
  , lholds n $ insertCommut                                                     -- 1
  , lholds n $ \(h,x) -> do insertxh <- insert' x h                             -- 2
                            return $ null (insertxh) == False
  , lholds n $ \(h,x) -> do insertxh <- insert' x h                             -- 3
                            return $ L.insert x (toList h) == toList (insertxh)
--,                       do deleteMinNil <- deleteMin' Nil                     -- 4
--                           return $ deleteMinNil == Nil
  , lholds n $ \(h,h1) -> do mergehh1 <- merge' h h1                            -- 5
                             mergeh1h <- merge' h1 h
                             return $ mergehh1 == mergeh1h
  , lholds n $ \h -> do mergehnil <- merge' h Nil                               -- 6
                        return $ mergehnil == h
  , lholds n $ \(h,h1,h2) -> do mergeh1h2 <- merge' h1 h2                       -- 7
                                mergehh1h2 <- merge' h mergeh1h2
                                mergehh2 <- merge' h h2
                                mergeh1hh2 <- merge' h1 mergehh2
                                return $ mergehh1h2 == mergeh1hh2
  , lholdE n $ \h -> if null h                                                  -- 8
                        then return True
                        else do merge_h_h <- merge' h h
                                return $ findMin (merge_h_h) == findMin h
  , lholds n $ \h -> do merge_h_h <- merge' h h                                 -- 9
                        return $ null merge_h_h == null h
  , lholds n $ \(h,h1) -> do merge_h_h1 <- merge' h h1                          -- 10
                             return $ null h && null h1 == null merge_h_h1
  , lholds n $ \(h,h1,x) -> do insert_x_h1 <- insert' x h1                      -- 11
                               merge_h_insert_x_h1 <- merge' h insert_x_h1
                               merge_h_h1 <- merge' h h1
                               insert_x_merge_h_h1 <- insert' x merge_h_h1
                               return $ merge_h_insert_x_h1 == insert_x_merge_h_h1
  , lholdE n $ \h -> if null h                                                  -- 12
                        then return True -- not (null h) ==>
                        else do deleteMin_h <- deleteMin' h
                                merge_h_deleteMin_h <- merge' h deleteMin_h
                                merge_h_h <- merge' h h
                                deleteMin_merge_h_h <- deleteMin' merge_h_h
                                return $ merge_h_deleteMin_h == deleteMin_merge_h_h
  , lholdE n $ \x -> do insert_x_Nil <- insert' x Nil                           -- 13
                        deleteMin_insert_x_Nil <- deleteMin' insert_x_Nil
                        return $ deleteMin_insert_x_Nil == Nil
  ]
  where insert' = curry lsMutateApply13
        deleteMin' = lsMutateApply23
        merge' = curry lsMutateApply33
        lholdE n = fmap errorToFalse . lholds n
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


sargs = args { functionNames = ["insert","deleteMin","merge"]
             , variableNames = ["ps","ps","ps"]
             , limitResults = Just 10
             }


data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 8       &= help "number of tests to run"
  , nMutants = 32      &= help "number of mutants to generate"
                       &= name "m"
  , testType = "int"   &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  }


main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (nMutants as) (nTests as)

run "bool"  = run' (uncurry insert) (deleteMin :: Heap Bool   -> Heap Bool  ) (uncurry merge)
run "bools" = run' (uncurry insert) (deleteMin :: Heap [Bool] -> Heap [Bool]) (uncurry merge)
run "int"   = run' (uncurry insert) (deleteMin :: Heap Int    -> Heap Int   ) (uncurry merge)

run' f g h m n = report3With sargs m $ propertyMap n f g h
