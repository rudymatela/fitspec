import FitSpec
import Test.Check
import Test.Check.Debug
import Control.Monad
import Data.List (sort,delete)


propertyMap :: Int
            -> ((Int,Int) -> Int)
            -> [[ ([Bool],Memo (Int,Int) Int) ]]
propertyMap n sum' =
  runListate sum' $ sequence $
    [ lholds n $ \(x,y) -> do xpy <- x -+- y
                              ypx <- y -+- x
                              return $ xpy == ypx
    , lholds n $ \(x,y,z) -> do xy   <-  x -+- y
                                xypz <- xy -+- z
                                yz   <-  y -+- z
                                xpyz <-  x -+- yz
                                return $ xypz == xpyz
    , lholds n $ \x -> do x' <- x -+- 0
                          return $ x' == x
    ]
  where (-+-) = curry lsMutateApply



sargs = args { functionNames = ["plus"]
             , variableNames = ["p"]
             , limitResults = Just 10
             }

main :: IO ()
main = do putStrLn "Properties over sum:"
          reportWith sargs 256 $ propertyMap 5 (uncurry (+))

          putLL 4 $ propertyMap 4 (uncurry (+))

putLL :: Show a => Int -> [[a]] -> IO ()
putLL n = putStr . unlines . map (unlines . map show) . take n
