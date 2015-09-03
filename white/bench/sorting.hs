{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import Test.Check
import Test.Check.Debug
import Test.Types
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

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , force :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 256     &= help "number of tests to run"
  , nMutants = 1024    &= help "number of mutants to generate"
                       &= name "m"
  , testType = "int"   &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , force    = False   &= help "force unevaluated arguments"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (nMutants as) (nTests as) (force as)

run :: String -> Int -> Int -> Bool -> IO ()
run "bool" = run' (sort :: [Bool]  -> [Bool])
run "int"  = run' (sort :: [Int]   -> [Int])
-- run "int2" = run' (sort :: [UInt2] -> [UInt2])
run "unit" = run' (sort :: [()]    -> [()])

run' f nmuts ntests force = reportWith sargs nmuts
                          $ propertyMap force ntests f
