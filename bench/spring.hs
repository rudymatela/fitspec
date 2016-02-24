import System.Console.CmdArgs hiding (args)
import FitSpec
import Data.List

type Add  a = a -> a -> a
type Prod a = a -> a -> a
type Ty a = ( Add  a
            , Prod a )

properties :: (Listable a, Show a, Eq a, Num a)
           => Add a
           -> Prod a
           -> [Property]
properties (+) (*) =
  [ property $ \x y   ->       x + y  ==  y + x
  , property $ \x y z ->  x + (y + z) == (x + y) + z
  , property $ \x     ->        x + 0 == x
  , property $ \x     ->        0 + x == x

  , property $ \x y   ->       x * y  ==  y * x
  , property $ \x y z ->  x * (y * z) == (x * y) * z
  , property $ \x     ->        x * 1 == x
  , property $ \x     ->        1 * x == x

  , property $ \x y z ->  x * (y + z) == (x * y) + (x * z)
  , property $ \x y z -> (y + z) * x  == (y * x) + (z * x)
  ]


fns :: Integral a => Ty a
fns = ((+),(*))


sargs :: (ShowMutable a, Listable a, Integral a, Show a, Read a)
      => Args (Ty a)
sargs =
  args { limitResults = Nothing
       , showPropertySets = unlines
       , names = [ "x + y", "x * y" ]
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
                     | False -- was useExtra
                     , s <- (+):(*):ems
                     , p <- (*):(+):ems
                     ]
       , nMutants = 1000
       , nTests   = 1000
       , timeout  =    0
       }

main :: IO ()
main = do
  let run f = mainWith sargs f (uncurry properties)
  ty <- typeArgument
  case ty of
    "int"   -> run (fns :: Ty Int)
    "int2"  -> run (fns :: Ty UInt2)
    "int3"  -> run (fns :: Ty UInt3)
    _       -> run (fns :: Ty UInt2)

(+++) :: (Show a, Read a, Integral a) => a -> a -> a
x +++ 0 = x
0 +++ y = y
x +++ y = read (show x ++ show (abs y))
