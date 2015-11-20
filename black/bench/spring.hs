{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import Data.List
import Test.Check
import Test.Types
import Test.Types.Mutate

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
  [ holds n $ \x y   ->       x + y  ==  y + x
  , holds n $ \x y z ->  x + (y + z) == (x + y) + z
  , holds n $ \x     ->        x + 0 == x
  , holds n $ \x     ->        0 + x == x

  , holds n $ \x y   ->       x * y  ==  y * x
  , holds n $ \x y z ->  x * (y * z) == (x * y) * z
  , holds n $ \x     ->        x * 1 == x
  , holds n $ \x     ->        1 * x == x

  , holds n $ \x y z ->  x * (y + z) == (x * y) + (x * z)
  , holds n $ \x y z -> (y + z) * x  == (y * x) + (z * x)
  ]


fns :: Integral a => Ty a
fns = ((+),(*))


sargs :: (ShowMutable a, Listable a, Integral a, Show a, Read a)
      => Bool -> Int -> Int
      -> Args (Ty a)
sargs useExtra nm nt =
  args { limitResults = Nothing
       , showPropertySets = unlines
       , callNames = [ "x + y", "x * y" ]
       , extraMutants =
           let ems = [ \x y -> x+y+1
                     , \x y -> x*y+x*y
                     , \x y -> x+y+x+y
                     , (+++)
                     , min
                     , max
                     -- another good example would be
                     -- || and && defined over integers
                     ]
           in drop 1 [ (s,p)
                     | useExtra
                     , s <- (+):(*):ems
                     , p <- (*):(+):ems
                     ]
       , nMutants = nm
       , nTestsF = const nt
       }


data CmdArguments = CmdArguments
  { nMutants_ :: Int
  , nTests :: Int
  , testType :: String
--, classify :: Bool
  , useExtraMutants :: Bool
  } deriving (Data,Typeable,Show,Eq)


arguments = CmdArguments
  { nTests   = 1000    &= help "number of tests to run"
  , nMutants_ = 1000    &= help "number of mutants to generate"
                       &= name "m"
  , testType = "int"   &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
--, classify = False   &= help "classify mutants, report extra column with fully evaluated ones (grey-box)"
  , useExtraMutants = False
                       &= help "pass extra manual mutants to the algorithm (only works for black-box version)"
                       &= name "e"
  }


main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (useExtraMutants as) (nMutants_ as) (nTests as)

run "int"  = run' (fns :: Ty Int)
run "int2" = run' (fns :: Ty UInt2)
run "int3" = run' (fns :: Ty UInt3)
run' fs em nm nt = reportWith (sargs em nm nt)
                              fs (uncurry . propertyMap)

(+++) :: (Show a, Read a, Integral a) => a -> a -> a
x +++ 0 = x
0 +++ y = y
x +++ y = read (show x ++ show (abs y))
