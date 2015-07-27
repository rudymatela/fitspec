import FitSpec
import Test.Check
import Test.Check.Debug
import Control.Monad
import Data.List (sort,delete)


ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

permutation :: Eq a => [a] -> [a] -> Bool
[]     `permutation` []    = True
(_:_)  `permutation` []    = False
[]     `permutation` (_:_) = False
(x:xs) `permutation` ys    = x `elem` ys  &&  xs `permutation` delete x ys


sortOrdered :: (Ord a, Listable a, Sized a)
            => [a]
            -> Listate [a] [a] Bool
sortOrdered xs = do sortxs <- lsMutateApply xs
                    return $ ordered (sortxs)

sortLength :: (Ord a, Listable a, Sized a)
           => [a]
           -> Listate [a] [a] Bool
sortLength xs = do sortxs <- lsMutateApply xs
                   return $ length (sortxs) == length xs


-- The force switch is only significant when testing "only" for the first
-- property (i.e.: commenting all other properties out)
propertyMap :: (Ord a, Listable a, Sized a)
            => Bool
            -> Int
            -> ([a] -> [a])
            -> [[ ([Bool],Memo [a] [a]) ]]
propertyMap force n sort' =
  runListate sort' $ sequence $
    [ lholds n $ \x -> do sortlx <- lsMutateApply [x]
                          return $ sortlx == [x]
    , lholds n sortOrdered
    , lholds n sortLength
  --, lholds n $ \xs     -> do sortxs <- lsMutateApply xs
  --                           sortsortxs <- lsMutateApply sortxs
  --                           return $ sortsortxs == sortxs
    , lholds n $ \(x,xs) -> do sortxs <- lsMutateApply xs
                               return $ elem x xs == elem x sortxs
    , lholds n $ \(x,xs) -> do sortxs <- lsMutateApply xs
                               return $ count x xs == count x sortxs
    , lholds n $ \xs     -> do sortxs <- lsMutateApply xs
                               return $ permutation xs sortxs
    ] ++ [ lholds n $ \xs -> do sortxs <- lsMutateApply xs
                                return $ True
         | force ]



sargs = args { functionNames = ["sort"]
             , variableNames = ["xs"]
             , limitResults = Just 10
             }

main :: IO ()
main = do reportWith sargs 1024 $ propertyMap False 256 (sort :: [Bool] -> [Bool])
          reportWith sargs 1024 $ propertyMap True  256 (sort :: [Bool] -> [Bool])
          reportWith sargs 1024 $ propertyMap False 256 (sort :: [Int] -> [Int])
          reportWith sargs 1024 $ propertyMap True  256 (sort :: [Int] -> [Int])
