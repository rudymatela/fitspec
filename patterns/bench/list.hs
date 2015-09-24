-- Note that this benchmark is currently not working as head and tail are not
-- total functions.  Support for total functions has to be added to Mutants.hs
{-# Language DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs hiding (args)
import Mutants
import Data.List

type Cons a = (a,[a]) -> [a]
type Head a = [a] -> a
type Tail a = [a] -> [a]
type Append a = ([a],[a]) -> [a]
type Ty a = ( Cons a
            , ( Head a
              , ( Tail a
                , Append a
                )
              )
            )

fns :: Ty a
fns = (uncurry (:),(head,(tail,(uncurry (++)))))

-- The property map
pmap :: (Eq a, Enumerable a)
     => Int
     -> Int
     -> Cons a
     -> Head a
     -> Tail a
     -> Append a
     -> Bool
pmap n k cons head tail append =
  [ runTests n $ \xs -> [] ++ xs == xs && xs == xs ++ []
  , runTests n $ \(x,xs) -> head (x-:xs) == x
  , runTests n $ \(x,xs) -> tail (x-:xs) == xs
  , runTests n $ \xs -> null (xs ++ xs) == null xs
  , runTests n $ \(xs,ys,zs) -> (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
  , runTests n $ \(x,xs,ys) -> x-:(xs ++ ys) == (x-:xs) ++ ys
  ] !! k
  where (-:) = curry cons
        (++) = curry append

pmap' :: (Eq a, Enumerable a)
      => Int
      -> [(Cons a, (Head a, (Tail a, (Append a)))) -> Bool]
pmap' n = map uncurry4' $ map (pmap n) [0..5]

data CmdArguments = CmdArguments
  { nMutants :: Int
  , nTests :: Int
  , testType :: String
  , typeLevelMutants :: Bool
  } deriving (Data,Typeable,Show,Eq)

arguments = CmdArguments
  { nTests   = 1000    &= help "number of tests to run"
  , nMutants = 3       &= help "mutant threshold"
                       &= name "m"
  , testType = "bool"  &= help "type to use"
                       &= name "type"
                       &= name "t"
                       &= explicit
  , typeLevelMutants = False
                       &= help "use type level mutants (mutate output of functions)"
                       &= name "y"
  }

main :: IO ()
main = do as <- cmdArgs arguments
          run (testType as) (typeLevelMutants as) (nMutants as) (nTests as)

run :: String -> Bool -> Int -> Int -> IO ()
run "int"   = run' (fns :: Ty Int)
run "bool"  = run' (fns :: Ty Bool)
run "bools" = run' (fns :: Ty [Bool])

run' f True  nMutants nTests = runM nMutants (pmap' nTests) f
run' f False nMutants nTests = runV (Just $ valid nTests) nMutants (pmap' nTests) f

uncurry4' :: (a -> b -> c -> d -> e) -> (a,(b,(c,d))) -> e
uncurry4' f (x,(y,(z,w))) = f x y z w

valid :: (Ord a, Enumerable a)
      => Int
      -> Ty a
      -> ()
valid n (f,(g,(h,i))) = v f `seq` v g `seq` v h `seq` v i where
  v a = runTests n ((`seq` True) . a) `seq` ()

