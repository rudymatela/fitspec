import FitSpec
import Test.Check
import Test.Check.Debug
import Control.Monad
import Data.Functor.Compose

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
x <**> y = getCompose (Compose x <*> Compose y)

headIsHead :: (Ord a, Listable a, Sized a)
           => (a,[a])
           -> Listate [a] [a] (Listate [a] a Bool)
headIsHead (x,xs) = (x ==) <$$> head (x:xs)
  where head = pure . lsMutateApply

tailIsTail :: (Ord a, Listable a, Sized a)
           => (a,[a])
           -> Listate [a] [a] (Listate [a] a Bool)
tailIsTail (x,xs) = (xs ==) <$$> tail (x:xs)
  where tail = fmap pure . lsMutateApply

headConsTail :: (Ord a, Listable a, Sized a)
             => (a,[a])
             -> Listate [a] [a] (Listate [a] a Bool)
headConsTail (x,xs) = (x:xs ==)
                 <$$> (:) <$$> head (x:xs) <**> tail (x:xs)
  where head = pure . lsMutateApply
        tail = fmap pure . lsMutateApply


nullProperty :: (Ord a, Listable a, Sized a)
             => (a,[a])
             -> Listate [a] [a] (Listate [a] a Bool)
nullProperty (x,xs) = (return . return) True

propertyMap :: (Ord a, Listable a, Sized a)
            => Int
            -> ([a] -> a)
            -> ([a] -> [a])
            -> [[ ([Bool],Memo [a] [a],Memo [a] a) ]]
propertyMap n head' tail' = runListate2 tail' head'
                          $ getCompose
                          $ traverse Compose [ lholds2 n headIsHead
                                             , lholds2 n tailIsTail
                                             , lholds2 n headConsTail
                                             , lholds2 n nullProperty
                                             ]

main :: IO ()
main = do putLL 5 $ propertyMap 5 (head :: [Bool] -> Bool) tail

