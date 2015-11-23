-- Example benchmark that mutation tests properties over the function id
{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
import Test.Check
import Test.Types
import Test.Types.Mutate

type Ty a = a -> a

-- The property map
pmap :: (Eq a, Show a, Listable a) => Int -> Ty a -> [Bool]
pmap n id =
  [ holds n $ \x -> id x == x
  , holds n $ \x -> id x == id x
  , holds n $ \x -> (id . id) x == x
  ]


sargs :: (Show a, Listable a, ShowMutable a)
      => Int -> Int
      -> Args (Ty a)
sargs nm nt = args
  { callNames = ["id x"]
  , limitResults = Just 10
  , nMutants = nm
  , nTestsF = const nt
  , minimumTime = 0
  }

csargs :: CArgs
csargs = cargs { functionNames = ["id"]
               , variableNames = ["x"]
               , nResults = Just 10
               }

data CmdArguments = CmdArguments
  { nMutants_ :: Int
  , nTests    :: Int
  , testType  :: String
  , classify  :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments :: CmdArguments
arguments = CmdArguments
  { nTests    = 1000   &= help "number of tests to run"
  , nMutants_ = 1000   &= help "number of mutants to generate"
                       &= name "m"
  , testType = "bool"  &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , classify = False   &= help "classify mutants, report extra column with fully evaluated ones (grey-box)"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (classify as) (nMutants_ as) (nTests as)

run :: String -> Bool -> Int -> Int -> IO ()
run "bool"  = run' (id :: Ty Bool)
run "bools" = run' (id :: Ty [Bool])
run "int"   = run' (id :: Ty Int)
run "int2"  = run' (id :: Ty UInt2)
run "int3"  = run' (id :: Ty UInt3)
run "unit"  = run' (id :: Ty ())
run' f False nm nt = reportWith  (sargs nm nt) f pmap
run' f True  nm nt = report1With csargs nm f (pmap nt)
