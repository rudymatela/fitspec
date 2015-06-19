import FitSpec
import Data.List
import Test.Check


propertyMap :: (Ord a, Show a, Listable a)
            => Int -> ([a] -> a) -> [Bool]
propertyMap n head' =
  [ holds n $ \x xs ->          head' (x:xs) == x
  ]


sargs :: (Listable a, Bounded a, Ord a) => Args ([a] -> a)
sargs = args { functionName = "head"
             , variableName = "xs"
             }


myhead :: [Int] -> Int
myhead [] = 10000000
myhead (x:xs) = x

main :: IO ()
main = do putStrLn "Integers:"
          reportWith sargs 10 (myhead) (propertyMap 3)

