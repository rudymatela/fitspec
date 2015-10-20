import FitSpec
import Data.List
import Test.Check

type Add  a = a -> a -> a
type Prod a = a -> a -> a
type Ty a = ( Add  a
            , Prod a )

propertyMap :: (Listable a, Show a, Eq a, Num a)
            => Int
            -> Add a
            -> Prod a
            -> [Bool]
propertyMap n (+) (*) =
  [ holds n $ \x y   ->      x + y  ==  y + x
  , holds n $ \x y z -> x + (y + z) == (x + y) + z
  , holds n $ \x     ->       x + 0 == x
  , holds n $ \x     ->       0 + x == x

  , holds n $ \x y   ->      x * y  ==  y * x
  , holds n $ \x y z -> x * (y * z) == (x * y) * z
  , holds n $ \x     ->       x * 1 == x
  , holds n $ \x     ->       1 * x == x

  , holds n $ \x y z -> x * (y + z) == (x * y) + (x * z)
  , holds n $ \x y z -> (y + z) * x == (y * x) + (z * x)
  ]


sargs :: (Integral a, Show a, Read a)
      => Bool -> Args (Ty a)
sargs useExtra =
  args { limitResults = Nothing
       , showPropertySets = unlines
       , extraMutants =
           if useExtra
             then [ ((*),             (+))
                  , (\x y -> x+y+1,   (*))
                  , ((+),             \x y -> x*y+x*y)
                  , (\x y -> x+y+x+y, (*))
                  , ((+++),           (*))
                  , ((+),             (+++))
                  , ((+++),           (+++))
                  , (min,             max)
                  , (max,             min)
                  -- another good example would be
                  -- || and && defined over integers
                  ]
             else []
       }


main :: IO ()
main = do putStrLn "### Sum and Product Ring ###"
          putStrLn "Simple finite mutations:"
          reportWith (sargs False) 1000
                     ((+),(*) :: Prod Int)
                     (uncurry $ propertyMap 1000)
          putStrLn "Also manual mutations:"
          reportWith (sargs True) 1000
                     ((+),(*) :: Prod Int)
                     (uncurry $ propertyMap 1000)


(+++) :: (Show a, Read a, Integral a) => a -> a -> a
x +++ 0 = x
0 +++ y = y
x +++ y = read (show x ++ show y)
