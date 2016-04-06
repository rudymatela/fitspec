-- Example benchmark that mutation tests properties over the function id
import System.Console.CmdArgs hiding (args)
import FitSpec

type Ty a = a -> a

-- The property map
properties :: (Eq a, Show a, Listable a) => Ty a -> [Property]
properties id =
  [ property $ \x -> id x == x
  , property $ \x -> id x == id x
  , property $ \x -> (id . id) x == x
  ]

sargs :: Args
sargs = args
  { names = ["id x"]
  , nMutants = 1000
  , nTests   = 2000
  , timeout  = 0
  }

main :: IO ()
main = do
  as <- getArgsWith sargs
  let run f = reportWith as f properties
  case concat (extra as) of
    "bool"  -> run (id :: Ty Bool)
    "bools" -> run (id :: Ty [Bool])
    "int"   -> run (id :: Ty Int)
    "int2"  -> run (id :: Ty UInt2)
    "int3"  -> run (id :: Ty UInt3)
    "unit"  -> run (id :: Ty ())
    ""      -> run (id :: Ty UInt2)
