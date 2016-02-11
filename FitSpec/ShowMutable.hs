-- | Show mutant variations
module FitSpec.ShowMutable
  ( ShowMutable (..)
  , mutantSEq
  )
where
-- The code in this module is very hacky and
-- probably needs refactoring in the future.
--
-- But hey!  It works for most cases.

import FitSpec.PrettyPrint
import Test.Check.Error (errorToNothing, Listable(..))
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

flatLambda :: String -> [([String],String)] -> String
flatLambda name []       = fname name
flatLambda _    [([],s)] = s
flatLambda name bs = (("\\" ++ unwords varnames ++ " -> ") `beside`)
                   $ "case " ++ showTuple varnames ++ " of\n"
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
                         else Just [([show x],showMutant (f x) (f' x))] -- tuple
                          -- in the above else clause, no error should be raised,
                          -- it is already catched by the enclosing case expression

instance (ShowMutable a, ShowMutable b) => ShowMutable (a,b) where
  mutantS (f,g) (f',g') = mutantS f f'
                       ++ mutantS g g'
  showMutant (f,g) (f',g') = showTuple [ showMutant f f'
                                       , showMutant g g' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c)
      => ShowMutable (a,b,c) where
  mutantS (f,g,h) (f',g',h') = mutantS f f'
                            ++ mutantS g g'
                            ++ mutantS h h'
  showMutant (f,g,h) (f',g',h') = showTuple [ showMutant f f'
                                            , showMutant g g'
                                            , showMutant h h' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d)
      => ShowMutable (a,b,c,d) where
  mutantS (f,g,h,i) (f',g',h',i') = mutantS f f'
                                 ++ mutantS g g'
                                 ++ mutantS h h'
                                 ++ mutantS i i'
  showMutant (f,g,h,i) (f',g',h',i') = showTuple [ showMutant f f'
                                                 , showMutant g g'
                                                 , showMutant h h'
                                                 , showMutant i i' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c,
          ShowMutable d, ShowMutable e)
      => ShowMutable (a,b,c,d,e) where
  mutantS (f,g,h,i,j) (f',g',h',i',j') = mutantS f f'
                                      ++ mutantS g g'
                                      ++ mutantS h h'
                                      ++ mutantS i i'
                                      ++ mutantS j j'
  showMutant (f,g,h,i,j) (f',g',h',i',j') = showTuple [ showMutant f f'
                                                      , showMutant g g'
                                                      , showMutant h h'
                                                      , showMutant i i'
                                                      , showMutant j j' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c,
          ShowMutable d, ShowMutable e, ShowMutable f)
      => ShowMutable (a,b,c,d,e,f) where
  mutantS (f,g,h,i,j,k) (f',g',h',i',j',k') = mutantS f f'
                                           ++ mutantS g g'
                                           ++ mutantS h h'
                                           ++ mutantS i i'
                                           ++ mutantS j j'
                                           ++ mutantS k k'
  showMutant (f,g,h,i,j,k) (f',g',h',i',j',k') = showTuple [ showMutant f f'
                                                           , showMutant g g'
                                                           , showMutant h h'
                                                           , showMutant i i'
                                                           , showMutant j j'
                                                           , showMutant k k' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g)
      => ShowMutable (a,b,c,d,e,f,g) where
  mutantS (f,g,h,i,j,k,l) (f',g',h',i',j',k',l') = mutantS f f'
                                                ++ mutantS g g'
                                                ++ mutantS h h'
                                                ++ mutantS i i'
                                                ++ mutantS j j'
                                                ++ mutantS k k'
                                                ++ mutantS l l'
  showMutant (f,g,h,i,j,k,l) (f',g',h',i',j',k',l') = showTuple
                                                    [ showMutant f f'
                                                    , showMutant g g'
                                                    , showMutant h h'
                                                    , showMutant i i'
                                                    , showMutant j j'
                                                    , showMutant k k'
                                                    , showMutant l l' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h)
      => ShowMutable (a,b,c,d,e,f,g,h) where
  mutantS (f,g,h,i,j,k,l,m) (f',g',h',i',j',k',l',m') = mutantS f f'
                                                     ++ mutantS g g'
                                                     ++ mutantS h h'
                                                     ++ mutantS i i'
                                                     ++ mutantS j j'
                                                     ++ mutantS k k'
                                                     ++ mutantS l l'
                                                     ++ mutantS m m'
  showMutant (f,g,h,i,j,k,l,m) (f',g',h',i',j',k',l',m') = showTuple
                                                         [ showMutant f f'
                                                         , showMutant g g'
                                                         , showMutant h h'
                                                         , showMutant i i'
                                                         , showMutant j j'
                                                         , showMutant k k'
                                                         , showMutant l l'
                                                         , showMutant m m' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i)
      => ShowMutable (a,b,c,d,e,f,g,h,i) where
  mutantS (f,g,h,i,j,k,l,m,n) (f',g',h',i',j',k',l',m',n') = mutantS f f'
                                                          ++ mutantS g g'
                                                          ++ mutantS h h'
                                                          ++ mutantS i i'
                                                          ++ mutantS j j'
                                                          ++ mutantS k k'
                                                          ++ mutantS l l'
                                                          ++ mutantS m m'
                                                          ++ mutantS n n'
  showMutant (f,g,h,i,j,k,l,m,n) (f',g',h',i',j',k',l',m',n') = showTuple
    [ showMutant f f'
    , showMutant g g'
    , showMutant h h'
    , showMutant i i'
    , showMutant j j'
    , showMutant k k'
    , showMutant l l'
    , showMutant m m'
    , showMutant n n' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j)
      => ShowMutable (a,b,c,d,e,f,h,g,i,j) where
  mutantS (f,g,h,i,j,k,l,m,n,o)
          (f',g',h',i',j',k',l',m',n',o') = mutantS f f'
                                         ++ mutantS g g'
                                         ++ mutantS h h'
                                         ++ mutantS i i'
                                         ++ mutantS j j'
                                         ++ mutantS k k'
                                         ++ mutantS l l'
                                         ++ mutantS m m'
                                         ++ mutantS n n'
                                         ++ mutantS o o'
  showMutant (f,g,h,i,j,k,l,m,n,o) (f',g',h',i',j',k',l',m',n',o') = showTuple
    [ showMutant f f'
    , showMutant g g'
    , showMutant h h'
    , showMutant i i'
    , showMutant j j'
    , showMutant k k'
    , showMutant l l'
    , showMutant m m'
    , showMutant n n'
    , showMutant o o' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j, ShowMutable k)
      => ShowMutable (a,b,c,d,e,f,g,h,i,j,k) where
  mutantS (f,g,h,i,j,k,l,m,n,o,p)
          (f',g',h',i',j',k',l',m',n',o',p') = mutantS f f'
                                            ++ mutantS g g'
                                            ++ mutantS h h'
                                            ++ mutantS i i'
                                            ++ mutantS j j'
                                            ++ mutantS k k'
                                            ++ mutantS l l'
                                            ++ mutantS m m'
                                            ++ mutantS n n'
                                            ++ mutantS o o'
                                            ++ mutantS p p'
  showMutant (f,g,h,i,j,k,l,m,n,o,p) (f',g',h',i',j',k',l',m',n',o',p') = showTuple
    [ showMutant f f'
    , showMutant g g'
    , showMutant h h'
    , showMutant i i'
    , showMutant j j'
    , showMutant k k'
    , showMutant l l'
    , showMutant m m'
    , showMutant n n'
    , showMutant o o'
    , showMutant p p' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j, ShowMutable k, ShowMutable l)
      => ShowMutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  mutantS (f,g,h,i,j,k,l,m,n,o,p,q)
          (f',g',h',i',j',k',l',m',n',o',p',q') = mutantS f f'
                                               ++ mutantS g g'
                                               ++ mutantS h h'
                                               ++ mutantS i i'
                                               ++ mutantS j j'
                                               ++ mutantS k k'
                                               ++ mutantS l l'
                                               ++ mutantS m m'
                                               ++ mutantS n n'
                                               ++ mutantS o o'
                                               ++ mutantS p p'
                                               ++ mutantS q q'
  showMutant (f,g,h,i,j,k,l,m,n,o,p,q) (f',g',h',i',j',k',l',m',n',o',p',q') = showTuple
    [ showMutant f f'
    , showMutant g g'
    , showMutant h h'
    , showMutant i i'
    , showMutant j j'
    , showMutant k k'
    , showMutant l l'
    , showMutant m m'
    , showMutant n n'
    , showMutant o o'
    , showMutant p p'
    , showMutant q q' ]

(+-+) :: String -> String -> String
cs +-+ ds = cs ++ " " ++ ds
infixr 5 +-+
