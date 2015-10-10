{-# Language DeriveDataTypeable #-}
import System.Console.CmdArgs hiding (args)
import FitSpec
import FitSpecC
import Data.List
import Test.Check
import Test.Types
import Test.Types.Mutate
import Utils (errorToFalse, uncurry4)

type Cons a = (a,[a]) -> [a]
type Head a = [a] -> a
type Tail a = [a] -> [a]
type Append a = ([a],[a]) -> [a]
type Ty a = ( Cons a
            , Head a
            , Tail a
            , Append a
            )

-- The property map
pmap :: (Eq a, Show a, Listable a)
     => Int
     -> Cons a
     -> Head a
     -> Tail a
     -> Append a
     -> [Bool]
pmap n cons head tail append =
  [ holds n $ \xs -> [] ++ xs == xs && xs == xs ++ []
  , holdE n $ \x xs -> head (x-:xs) == x  -- mutated (:) might return an empty list
  , holdE n $ \x xs -> tail (x-:xs) == xs -- mutated (:) might return an empty list
  , holds n $ \xs -> null (xs ++ xs) == null xs
  , holds n $ \xs ys zs -> (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
  , holds n $ \x xs ys -> x-:(xs ++ ys) == (x-:xs) ++ ys
  ]
  where (-:) = curry cons
        (++) = curry append
        holdE n = errorToFalse . holds n


fns :: Ty a
fns = (uncurry (:),head,tail,uncurry (++))

sargs :: Listable a => Bool -> Args (Ty a)
sargs em = args
             { callNames = ["(:) x xs","head xs","tail xs","(++) xs ys"]
             , limitResults = Just 30
             , extraMutants = takeWhile (const em)
                              [ (uncurry (:),head,tail,uncurry (++-))
                              , (uncurry (:),head,tail,uncurry (++--))
                              ]
             }

csargs = cargs { functionNames = [":","head","tail","++"]
               , variableNames = ["p","xs","xs","p"]
               , nResults = Just 30
               }

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , classify :: Bool
  , useExtraMutants :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 1000    &= help "number of tests to run"
  , nMutants = 1000    &= help "number of mutants to generate"
                       &= name "m"
  , testType = "bool"  &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , classify = False   &= help "classify mutants, report extra column with fully evaluated ones (grey-box)"
  , useExtraMutants = False
                       &= help "pass extra manual mutants to the algorithm (only works for black-box version)"
                       &= name "e"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (classify as) (useExtraMutants as) (nMutants as) (nTests as)

run :: String -> Bool -> Bool -> Int -> Int -> IO ()
run "int"   = run' (fns :: Ty Int)
run "int2"  = run' (fns :: Ty UInt2)
run "int3"  = run' (fns :: Ty UInt3)
run "bool"  = run' (fns :: Ty Bool)
run "bools" = run' (fns :: Ty [Bool])
run "unit"  = run' (fns :: Ty ())
run' f False em nm nt = reportWith (sargs em) nm f (uncurry4 $ pmap nt)
-- run' f True  em nm nt = report1With csargs nm f (pmap nt) -- TODO


-- Some manual mutants
(++-) :: [a] -> [a] -> [a]
xs ++- ys = []

(++--) :: [a] -> [a] -> [a]
xs ++-- ys = if length xs > length ys
               then xs
               else ys
