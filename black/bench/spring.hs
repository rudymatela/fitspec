import FitSpec
import Data.List
import Test.Check

mapPropsR :: ((Int,Int)->Int, (Int,Int)->Int) -> Int -> [Bool]
mapPropsR (sum',product') n =
  [ holds n $ \x y   ->        x -+- y  ==  y -+- x
  , holds n $ \x y z -> x -+- (y -+- z) == (x -+- y) -+- z
  , holds n $ \x     ->         x -+- 0 == x

  , holds n $ \x y   ->        x -*- y  ==  y -*- x
  , holds n $ \x y z -> x -*- (y -*- z) == (x -*- y) -*- z
  , holds n $ \x     ->         x -*- 1 == x

  , holds n $ \x y z -> x -*- (y -+- z) == (x -*- y) -+- (x -*- z)
  , holds n $ \x y z -> (y -+- z) -*- x == (y -*- x) -+- (z -*- x)
  ]
  where (-+-) = curry sum'
        (-*-) = curry product'


sargs :: Args ((Int,Int)->Int,(Int,Int)->Int)
sargs = args { extraMutants = [ (uncurry (*),     uncurry (+))
                              , (\(x,y) -> x+y+1, uncurry (*))
                              , (uncurry (+),     \(x,y) -> x*y*2)
                              , (\(x,y) -> (x+y)*2, uncurry (*))

                              , (uncurry (+++),   uncurry (*))
                              , (uncurry (+),     uncurry (+++))
                              , (uncurry (+++),   uncurry (+++))
                              , (uncurry min,     uncurry max)
                              , (uncurry max,     uncurry min)
                              -- another good example would be 
                              -- || and && defined over integers
                              ]
             }


main :: IO ()
main = do putStrLn "### Sum and Product Ring ###"
          putStrLn "Simple finite mutations:"
          report           500 (uncurry (+),uncurry (*)) (`mapPropsR` 200)
          putStrLn "Also manual mutations:"
          reportWith sargs 500 (uncurry (+),uncurry (*)) (`mapPropsR` 200)


(+++) :: Int -> Int -> Int
x +++ 0 = x
0 +++ y = y
x +++ y = read (show x ++ show y)
