import FitSpec
import Test.Check
import Control.Monad
import Data.Functor.Compose

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
x <**> y = getCompose (Compose x <*> Compose y)

headIsHead :: (Ord a, Listable a, Sized a)
           => (a,[a])
           -> Listate2  [a] [a]  [a] a  Bool
headIsHead (x,xs) = (x ==) <$> head (x:xs)
  where head = lsMutateApply22

tailIsTail :: (Ord a, Listable a, Sized a)
           => (a,[a])
           -> Listate2  [a] [a]  [a] a  Bool
tailIsTail (x,xs) = (xs ==) <$> tail (x:xs)
  where tail = lsMutateApply12

headConsTail :: (Ord a, Listable a, Sized a)
             => (a,[a])
             -> Listate2  [a] [a]  [a] a  Bool
headConsTail (x,xs) = do headxxs <- head (x:xs)
                         tailxxs <- tail (x:xs)
                         return $ (x:xs) == (headxxs:tailxxs)
  where head = lsMutateApply22
        tail = lsMutateApply12


nullProperty :: (Ord a, Listable a, Sized a)
             => (a,[a])
             -> Listate2  [a] [a]  [a] a  Bool
nullProperty (x,xs) = return True

propertyMap :: (Ord a, Listable a, Sized a)
            => Int
            -> ([a] -> a)
            -> ([a] -> [a])
            -> [[ ([Bool],Memo [a] [a],Memo [a] a) ]]
propertyMap n head' tail' = runListate2 tail' head'
                          $ sequence [ lholds n headIsHead
                                     , lholds n tailIsTail
                                     , lholds n headConsTail
                                     , lholds n nullProperty
                                     ]


sargs = args { functionNames = ["head","tail"]
             , variableNames = ["xs","xs"]
             , limitResults = Just 10
             }

main :: IO ()
main = do putLL 5 $ propertyMap 5 (head :: [Bool] -> Bool) tail
          report2With sargs 8 $ propertyMap 8 (head :: [Bool] -> Bool) tail

putLL :: Show a => Int -> [[a]] -> IO ()
putLL n = putStr . unlines . map (unlines . map show) . take n
