import FitSpec
import Data.List
import Test.Check

propertyMap :: (Listable a, Show a, Eq a, Num a)
            => Int
            -> ((a,a)->a, (a,a)->a) -> [Bool]
propertyMap n (sum',product') =
  [ holds n $ \x y   ->        x -+- y  ==  y -+- x
  , holds n $ \x y z -> x -+- (y -+- z) == (x -+- y) -+- z
  , holds n $ \x     ->         x -+- 0 == x
  , holds n $ \x     ->         0 -+- x == x

  , holds n $ \x y   ->        x -*- y  ==  y -*- x
  , holds n $ \x y z -> x -*- (y -*- z) == (x -*- y) -*- z
  , holds n $ \x     ->         x -*- 1 == x
  , holds n $ \x     ->         1 -*- x == x

  , holds n $ \x y z -> x -*- (y -+- z) == (x -*- y) -+- (x -*- z)
  , holds n $ \x y z -> (y -+- z) -*- x == (y -*- x) -+- (z -*- x)
  ]
  where (-+-) = curry sum'
        (-*-) = curry product'


sargs :: (Integral a, Show a, Read a)
      => Bool -> Args ((a,a)->a,(a,a)->a)
sargs useExtra =
  args { limitResults = Nothing
       , showPropertySets = unwords
       , extraMutants =
           if useExtra
             then [ (uncurry (*),     uncurry (+))
                  , (\(x,y) -> x+y+1, uncurry (*))
                  , (uncurry (+),     \(x,y) -> x*y+x*y)
                  , (\(x,y) -> x+y+x+y, uncurry (*))
                  , (uncurry (+++),   uncurry (*))
                  , (uncurry (+),     uncurry (+++))
                  , (uncurry (+++),   uncurry (+++))
                  , (uncurry min,     uncurry max)
                  , (uncurry max,     uncurry min)
                  -- another good example would be
                  -- || and && defined over integers
                  ]
             else []
       }


main :: IO ()
main = do putStrLn "### Sum and Product Ring ###"
          putStrLn "Simple finite mutations:"
          reportWith (sargs False) 1000
                     (uncurry (+) :: (Int,Int)->Int,uncurry (*))
                     (propertyMap 1000)
          putStrLn "Also manual mutations:"
          reportWith (sargs True) 1000
                     (uncurry (+) :: (Int,Int)->Int,uncurry (*))
                     (propertyMap 1000)


(+++) :: (Show a, Read a, Integral a) => a -> a -> a
x +++ 0 = x
0 +++ y = y
x +++ y = read (show x ++ show y)
