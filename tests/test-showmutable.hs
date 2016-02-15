{-# LANGUAGE NoMonomorphismRestriction #-}
import System.Exit (exitFailure)
import Data.List (elemIndices, sort)
import Data.Tuple (swap)

import FitSpec hiding (showMutantN)
import FitSpec.ShowMutable (showMutantN)
import FitSpec.PrettyPrint
import Test.Check.Error (errorToNothing)

(-:) :: a -> a -> a
(-:) = const

(->:) :: (a -> b) -> a -> (a -> b)
(->:) = const

(->>:) :: (a -> b -> c) -> b -> (a -> b -> c)
(->>:) = const

int :: Int
int = undefined

bool :: Bool
bool = undefined

char :: Char
char = undefined

string :: String
string = undefined

main :: IO ()
main =
  case elemIndices False (tests 200) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

-- Below, some of the tests are marked as failing:
-- They simply fail because there are too many values
-- being tested (the showMutant function only approximates
-- the printing of a mutant)
-- There is a TODO in the other file related  to this
-- to allow configuration of how many values should be
-- tried when enumerating the mutants.
tests n =
  [ True

  , holds n $ prop_0 ->: int
  , holds n $ prop_0 ->: bool
  , holds n $ prop_0 ->: char
  , holds n $ prop_0 ->: string
  , fails n $ prop_0 ->: (int,int)

  , holds n $ prop_00 ->: (int,int)
  , holds n $ prop_00 ->: (bool,int)
  , holds n $ prop_00 ->: (int,bool)
  , holds n $ prop_00 ->: (string,char)
  , holds n $ prop_00 ->: (char,string)

  , h1 $ id ->: int
  , h1 $ id ->: bool
  , h1 $ id ->: (int,int)
  , h1 $ id ->: (bool,int)
  , h1 $ id ->: (int,bool)
  , h1 $ id ->: (bool,bool)
  , h1 $ id ->: (int,int,int)
  , h1 $ id ->: (int,(int,int))
  , h1 $ id ->: ((int,int),int)
  , h1 $ const True     ->: int  -- fails (500 tests)!
  , h1 $ const (0::Int) ->: bool
  , h1 $ swap ->: (int,bool)
  , h1 $ swap ->: (bool,int)
  , h1 $ swap ->: (int,(int,int))
  , h1 $ swap ->: ((int,int),int)

  , h2 $ const ->: int  ->>: int
  , h2 $ const ->: int  ->>: bool
  , h2 $ const ->: bool ->>: int  -- fails (1000 tests)!
  , h2 $ (+) ->: int
  , h2 $ (*) ->: int
  , h2 $ (&&)
  , h2 $ (||)
  , h2 $ (:) ->: int  -- fails (2000 tests)!
  , h2 $ (++) ->: [int]  -- fails (3000 tests)!

  , h11 (id ->: int)  (id ->: bool)
  , h11 (id ->: bool) (id ->: int)
  , h11 (swap ->: ((int,int),int)) (swap ->: (int,(int,int)))
  ]
  where h1 = holds n . prop_1
        h2 = holds n . prop_2
        h11 f = holds n . prop_11 f

-- prop_N, asserts the format of a mutation of a value of N arguments
-- prop_MN, asserts the format of a pair of values of M and N arguments
-- prop_MNO, asserts the format of a triple of values

prop_0 :: (Eq a, Show a, Listable a, ShowMutable a)
        => a -> a -> Bool
prop_0 x x' | x == x'   = s == "x"
            | otherwise = s == show x'
  where s = showMutantN ["x"] x x'

prop_00 :: ( Eq a, Show a, Listable a, ShowMutable a
           , Eq b, Show b, Listable b, ShowMutable b )
        => (a,b) -> (a,b) -> Bool
prop_00 (x,y) (x',y') | x == x' && y == y' = s == "(x,y)"
                      | x == x'            = s == "(x," ++ show y' ++ ")"
                      | y == y'            = s == "(" ++ show x' ++ ",y)"
                      | otherwise          = s == "(" ++ show x'
                                               ++ "," ++ show y' ++ ")"
  where s = showMutantN ["x","y"] (x,y) (x',y')

prop_1 :: ( Eq a, Show a, Listable a, ShowMutable a
          , Eq b, Show b, Listable b, ShowMutable b )
        => (a->b) -> a -> b -> Bool
prop_1 f x y = y /= f x
           ==> showMutantN ["f x"] f (mutate f x y)
            == showMutant1 "f" x y
-- TODO: fix above property that fails for 'showMutant'
--       instead of showMutantN ["f x"], see:
--
-- > > putStrLn $ showMutant id (mutate id (0::Int) (1::Int))
-- > \x -> case x of
-- >         0 -> 1
-- >         _ -> f

prop_11 :: ( Eq a, Show a, Listable a, ShowMutable a
           , Eq b, Show b, Listable b, ShowMutable b
           , Eq c, Show c, Listable c, ShowMutable c
           , Eq d, Show d, Listable d, ShowMutable d )
        => (a->b) -> (c->d) -> a -> b -> c -> d -> Bool
prop_11 f g x y z w = y /= f x && w /= g z
                  ==> showMutantN ["f x","g x"] (f,g) (mutate f x y, mutate g z w)
                   == showTuple [showMutant1 "f" x y, showMutant1 "g" z w]

prop_2 :: ( Eq a, Show a, Listable a, ShowMutable a
          , Eq b, Show b, Listable b, ShowMutable b
          , Eq c, Show c, Listable c, ShowMutable c )
        => (a->b->c) -> a -> b -> c -> Bool
prop_2 f x y z = z /= f x y
             ==> showMutantN ["f x y"] f (mutate2 f x y z)
              == showMutant2 "f" x y z

mutate :: Eq a => (a -> b) -> a -> b -> (a -> b)
mutate f x y x' | x' == x   = y
                | otherwise = f x'

mutate2 :: (Eq a, Eq b) => (a -> b -> c) -> a -> b -> c -> (a -> b -> c)
mutate2 f x y z = curry (mutate (uncurry f) (x,y) z)

-- | Show a mutant of a function of one argument
showMutant1 :: (Show a, Show b)
            => String -> a -> b -> String
showMutant1 f x y = "\\x -> case x of\n"
                 ++ "        " ++ show x ++ " -> " ++ show y ++ "\n"
                 ++ "        _ -> " ++ f ++ " x\n"

-- | Show a mutant of a function of two arguments
showMutant2 :: (Show a, Show b, Show c)
            => String -> a -> b -> c -> String
showMutant2 f x y z = "\\x y -> case (x,y) of\n"
                   ++ "          (" ++ show x ++ "," ++ show y ++ ") -> "
                                    ++ show z ++ "\n"
                   ++ "          _ -> " ++ f ++ " x y\n"