module Mutate.Show
  ( ShowMutable (..)
  , mutantSEq
  )
where

import PPPrint
import Utils (errorToNothing)
import Test.Check
import Data.Maybe (mapMaybe,isNothing)
import Control.Monad (join)
import Data.List (intercalate)
import Data.Char (isLetter)

-- | Default function name, when none given
defFn :: String
defFn  = head defFns

defFns :: [String]
defFns = ["f","g","h","i"] ++ map (++"'") defFns

defVn :: String
defVn = head defVns

-- | Default variable names, when none given
defVns :: [String]
defVns = ["x","y","z","w"] ++ map (++"'") defVns


{-
  Atomic value mutants:
    Int, mutated:     [[([],10)]] 
    Int, not mutated: [[]]
    (),  not mutated: [[]]

  Functional value mutants:
    Int -> Int, not mutated:  [[]]
    Int -> Int, mutated:      [[([10],10)]]
    Int -> Int -> Int, mutated:  [[([10,10,10],10)]]

  Paired mutants:
    (Int,Int):    [[([],10)],[]]

  invalid:
    []
-}
flatLambdas :: [String] -> [[([String],String)]] -> String
flatLambdas names = showTuple . zipWith flatLambda (names ++ defFns)

-- TODO: Pretty print infix operators: `1 + 2` instead of `(+) 1 2`
flatLambda :: String -> [([String],String)] -> String
flatLambda name []       = fname name
flatLambda _    [([],s)] = s
flatLambda name bs = (("\\" ++ (unwords varnames) ++ " -> ") `beside`)
                   $ "case " ++ (showTuple varnames) ++ " of\n"
                  ++ "  " `beside` cases
  where
    cases = unlines (map (\(as,r) -> showTuple as ++ " -> " ++ r) bs)
         ++ "_ -> " ++ name
    varnames = zipWith const
                       (vnames name ++ defVns)
                       (fst $ head bs)


-- | Separate function from variable names in a simple Haskell expr.
--
-- > fvarnames "f x y" == ("f",["x","y"])
-- > fvarnames "x + y" == ("(+)",["x","y"])
-- > fvarnames "aa bb cc dd" == ("aa",["bb","cc","dd"])
--
-- When there are three lexemes, the function checks for a potential infix
-- operator in the middle.
fvnames :: String -> (String,[String])
fvnames = fvns' . words
  where fvns' :: [String] -> (String,[String])
        fvns' [a,b:bs,c] | b /= '(' && not (isLetter b)
                         = if b == '`'
                             then (init bs,[a,c])         -- `o` -> o
                             else ('(':b:bs ++ ")",[a,c]) --  +  -> (+)
        fvns' []         = (defFn,[])
        fvns' (f:vs)     = (f,vs)

-- | Separate the function name from a simple Haskell expression.
--   See the docs for 'fvnames'
fname :: String -> String
fname = fst . fvnames

-- | Separate the variable names from a simple Haskell expression.
--   See the docs for 'fvnames'
vnames :: String -> [String]
vnames = snd . fvnames

class ShowMutable a where
  mutantS :: a -> a -> [[([String],String)]]

  showMutantN :: [String] -> a -> a -> String
  showMutantN ns f = flatLambdas ns . mutantS f

  showMutant :: a -> a -> String
  showMutant = showMutantN []

mutantSEq :: (Eq a, Show a)
          => a -> a -> [[([String],String)]]
mutantSEq x x' = if x == x'
                   then [ [] ]
                   else [ [([],show x')] ]

instance ShowMutable ()   where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable Int  where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable Char where mutantS = mutantSEq; showMutant _ = show
instance ShowMutable Bool where mutantS = mutantSEq; showMutant _ = show
instance (Eq a, Show a) => ShowMutable [a]       where mutantS = mutantSEq; showMutant _ = show
instance (Eq a, Show a) => ShowMutable (Maybe a) where mutantS = mutantSEq; showMutant _ = show


instance (Listable a, Show a, ShowMutable b) => ShowMutable (a->b) where
  -- TODO: maybe somehow let the user provide how many values should be tried
  -- when printing the mutant
  mutantS f f' = (:[])
               . take 10
               . concat
               . mapMaybe bindingsFor
               . take 200
               $ list
    where
   -- bindingsFor :: a -> Maybe [([String],String)]
      bindingsFor x =
        case errorToNothing $ mutantS (f x) (f' x) of
          Nothing   -> Nothing -- error
          Just []   -> Nothing -- returned (), null mutant
          Just [[]] -> Nothing -- null mutant
          Just [bs] -> Just $ map (\(xs,y) -> (show x:xs,y)) bs -- valid mutant, prepend x arg
          Just ys  ->  if all null ys
                         then Nothing
                         else Just $ [([show x],showMutant (f x) (f' x))] -- tuple
                          -- in the above else clause, no error should be raised,
                          -- it is already catched by the enclosing case expression

instance (ShowMutable a, ShowMutable b) => ShowMutable (a,b) where
  mutantS (f,g) (f',g') = mutantS f f'
                       ++ mutantS g g'
  showMutant (f,g) (f',g') = showTuple [ showMutant f f'
                                       , showMutant g g' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c) => ShowMutable (a,b,c) where
  mutantS (f,g,h) (f',g',h') = mutantS f f'
                            ++ mutantS g g'
                            ++ mutantS h h'
  showMutant (f,g,h) (f',g',h') = showTuple [ showMutant f f'
                                            , showMutant g g'
                                            , showMutant h h' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d) => ShowMutable (a,b,c,d) where
  mutantS (f,g,h,i) (f',g',h',i') = mutantS f f'
                                 ++ mutantS g g'
                                 ++ mutantS h h'
                                 ++ mutantS i i'
  showMutant (f,g,h,i) (f',g',h',i') = showTuple [ showMutant f f'
                                                 , showMutant g g'
                                                 , showMutant h h'
                                                 , showMutant i i' ]

(+-+) :: String -> String -> String
cs +-+ ds = cs ++ " " ++ ds
infixr 5 +-+
