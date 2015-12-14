-- Example benchmark that mutation tests properties over the function id
{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec.Main
import Test.Check
import Test.Types
import Test.Types.Mutate

type Ty a = a -> a

-- The property map
properties :: (Eq a, Show a, Listable a) => Ty a -> [Property]
properties id =
  [ property $ \x -> id x == x
  , property $ \x -> id x == id x
  , property $ \x -> (id . id) x == x
  ]

sargs :: (Show a, Listable a, ShowMutable a)
      => Args (Ty a)
sargs = args
  { callNames = ["id x"]
  , limitResults = Just 10
  , nMutants = 1000
  , minimumTime = 0
  }

main :: IO ()
main = do
  let run f = mainWith sargs f properties
  ty <- typeArgument
  case ty of
    "bool"  -> run (id :: Ty Bool)
    "bools" -> run (id :: Ty [Bool])
    "int"   -> run (id :: Ty Int)
    "int2"  -> run (id :: Ty UInt2)
    "int3"  -> run (id :: Ty UInt3)
    "unit"  -> run (id :: Ty ())
    _       -> run (id :: Ty UInt2)
