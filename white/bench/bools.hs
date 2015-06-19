import FitSpec
import Test.Check
import Test.Check.Debug
import Control.Monad


involuntory :: (Ord a, Listable a, Sized a)
            => a
            -> Listate a a Bool
involuntory x = do fx <- lsMutateApply x
                   ffx <- lsMutateApply fx
                   return $ ffx == x

notid :: (Ord a, Listable a, Sized a)
      => a
      -> Listate a a Bool
notid x = do fx <- lsMutateApply x
             return $ fx /= x



commutative :: (Ord a, Eq b, Listable b, Sized a)
            => (a,a)
            -> Listate (a,a) b Bool
commutative (x,y) = (==) <$> x `op` y <*> y `op` x
  where op = lsMutateApply2

allidempotent :: (Ord a, Listable a, Sized a)
              => a
              -> Listate (a,a) a Bool
allidempotent x = (x ==) <$> x `op` x
  where op = lsMutateApply2

associative :: (Listable a, Ord a, Sized a)
            => (a,a,a)
            -> Listate (a,a) a Bool
associative (x,y,z) = (==) <$> (x % y >>= (% z)) <*> ((x %) =<< (y % z))
  where (%) = lsMutateApply2


checknot = putLL 100
         $ runListate not (sequence [ lholds 100 notid
                                    , lholds 100 involuntory ])

checkand = putLL 100
         $ runListate (uncurry (&&)) (sequence [ lholds 100 commutative
                                               , lholds 100 allidempotent
                                               , lholds 100 associative ])

main :: IO ()
main = do putStrLn "Not results:"
          checknot
          putStrLn "And results:"
          checkand
