-- |
-- Module      : Test.FitSpec.ShowMutable
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Exports a typeclass to show mutant variations.
module Test.FitSpec.ShowMutable
  ( ShowMutable (..)
  , mutantSEq
  , showMutantAsTuple
  , showMutantNested
  , showMutantDefinition
  , showMutantBindings
  , MutantS ()
  , mutantSTuple
  )
where

import Test.FitSpec.PrettyPrint
import Test.LeanCheck.Error (errorToNothing, Listable(..))
import Data.Maybe (mapMaybe,isNothing)
import Control.Monad (join)
import Data.List (intercalate,tails)
import Data.Char (isLetter)
import Data.Ratio (Ratio)
import Data.Word (Word) -- for GHC <= 7.10


-- | Show a Mutant as a tuple of lambdas.
--
-- > > putStrLn $ showMutantAsTuple ["p && q","not p"] ((&&),not) ((||),id)
-- > ( \p q -> case (p,q) of
-- >            (False,False) -> True
-- >            _ -> p && q
-- > , \p -> case p of
-- >           False -> False
-- >           True -> True
-- >           _ -> not p )
--
-- Can be easily copy pasted into an interactive session for manipulation.
-- On GHCi, use @:{@ and @:}@ to allow multi-line expressions and definitions.
showMutantAsTuple :: ShowMutable a => [String] -> a -> a -> String
showMutantAsTuple names f f' = showMutantSAsTuple names
                             $ flatten
                             $ mutantS f f'

-- | Show a Mutant as the list of bindings that differ from the original
--   function(s).
--
-- > > putStrLn $ showMutantBindings ["p && q","not p"] ((&&),not) ((==),id)
-- > False && False = True
-- > not False = False
-- > not True  = True
--
-- Can possibly be copied into the source of the original function for
-- manipulation.
showMutantBindings :: ShowMutable a => [String] -> a -> a -> String
showMutantBindings names f f' = showMutantSBindings False names
                              $ flatten
                              $ mutantS f f'

-- | Show a Mutant as a new complete top-level definition, with a prime
-- appended to the name of the mutant.
--
-- > > putStrLn $ showMutantDefinition ["p && q","not p"] ((&&),not) ((==),id)
-- > False &&- False = True
-- > p     &&- q     = p && q
-- > not' False = False
-- > not' True  = True
-- > not' p     = not p
showMutantDefinition :: ShowMutable a => [String] -> a -> a -> String
showMutantDefinition names f f' = showMutantSBindings True names
                               $ flatten
                               $ mutantS f f'

-- | Show a Mutant as a tuple of nested lambdas.
-- Very similar to 'showMutantAsTuple', but the underlying data structure is
-- not flatten: so the output is as close as possible to the underlying
-- representation.
showMutantNested :: ShowMutable a => [String] -> a -> a -> String
showMutantNested names f f' = showMutantSAsTuple names
                            $ mutantS f f'

-- | Show a Mutant without providing a default name.
-- An alias for @showMutantAsTuple []@.
showMutant :: ShowMutable a => a -> a -> String
showMutant = showMutantAsTuple []


-- | Default function names (when none given):
--
-- > f g h f' g' h' f'' g'' h''
defaultFunctionNames :: [String]
defaultFunctionNames = ["f","g","h"] ++ map (++"'") defaultFunctionNames

-- | Default names in a call (function and variables):
--
-- > f x y z w x' y' z' w' x'' y'' z'' w'' ...
defaultNames :: [String]
defaultNames = head defaultFunctionNames : defVarNames
  where defVarNames = ["x","y","z","w"] ++ map (++"'") defVarNames


-- | Types that can have their mutation shown.
-- Has only one function 'mutantS' that returns a simple AST ('MutantS')
-- representing the mutant.  A standard implementation of 'mutantS' for 'Eq'
-- types is given by 'mutantSEq'.
class ShowMutable a where
  mutantS :: a -> a -> MutantS

-- | For a given type @Type@ instance of @Eq@ and @Show@,
-- define the 'ShowMutable' instance as:
--
-- > instance ShowMutable Type
-- >   where mutantS = mutantSEq
mutantSEq :: (Eq a, Show a)
          => a -> a -> MutantS
mutantSEq x x' = if x == x'
                    then Unmutated $ show x
                    else Atom      $ show x'

instance ShowMutable ()   where mutantS = mutantSEq
instance ShowMutable Int  where mutantS = mutantSEq
instance ShowMutable Integer where mutantS = mutantSEq
instance ShowMutable Char where mutantS = mutantSEq
instance ShowMutable Bool where mutantS = mutantSEq
instance (Eq a, Show a) => ShowMutable [a]       where mutantS = mutantSEq
instance (Eq a, Show a) => ShowMutable (Maybe a) where mutantS = mutantSEq
instance (Eq a, Show a, Eq b, Show b) => ShowMutable (Either a b)
  where mutantS = mutantSEq
instance (Eq a, Show a, Integral a) => ShowMutable (Ratio a)
  where mutantS = mutantSEq
instance ShowMutable Float    where mutantS = mutantSEq
instance ShowMutable Double   where mutantS = mutantSEq
instance ShowMutable Ordering where mutantS = mutantSEq
instance ShowMutable Word     where mutantS = mutantSEq

instance (Listable a, Show a, ShowMutable b) => ShowMutable (a->b) where
  -- TODO: let the user provide how many values should be tried when printing
  mutantS f f' = Function
               . take 10
               . filter (not . isUnmutated . snd)
               . mapMaybe bindingFor
               . take 200
               $ list
    where bindingFor x = fmap ((,) [show x])
                       $ errorToNothing (mutantS (f x) (f' x))

instance (ShowMutable a, ShowMutable b) => ShowMutable (a,b) where
  mutantS (f,g) (f',g') = Tuple [ mutantS f f'
                                , mutantS g g' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c)
      => ShowMutable (a,b,c) where
  mutantS (f,g,h) (f',g',h') = Tuple [ mutantS f f'
                                     , mutantS g g'
                                     , mutantS h h' ]


-- | (Show) Structure of a mutant.
-- This format is intended for processing then pretty-printing.
data MutantS = Unmutated String
             | Atom String
             | Tuple [MutantS]
             | Function [([String],MutantS)]
  deriving Show

-- | Check if a 'MutantS' is null
isUnmutated :: MutantS -> Bool
isUnmutated (Unmutated _) = True
isUnmutated (Tuple ms)    = all isUnmutated ms
isUnmutated (Function bs) = all (isUnmutated . snd) bs
isUnmutated _             = False

-- | Check if a 'MutantS' is a function.
isFunction :: MutantS -> Bool
isFunction (Function _) = True
isFunction _            = False

-- Flatten a MutantS by merging nested 'Function's.
flatten :: MutantS -> MutantS
flatten (Tuple ms) = Tuple $ map flatten ms
flatten (Function [([],s)])  = flatten s
flatten (Function (([],s):_)) = error "flatten: ambiguous value"
flatten (Function bs) = let bs' = map (mapSnd flatten) bs in
  if any (not . isFunction . snd) bs'
    then Function bs'
    else Function
       $ take 10
       $ concatMap (\(as,Function bs'') -> map (mapFst (as++)) bs'') bs'
flatten m = m


-- | Show a nameless mutant.
-- Functions should not (but can) be shown using this.
showMutantS :: MutantS -> String
showMutantS (Unmutated s) = s
showMutantS (Atom s)      = s
showMutantS (Tuple ms)    = showTuple $ map showMutantS ms
showMutantS (Function bs) = showLambda ["??"] bs

-- | Show top-level (maybe tuple) named 'MutantS' as a tuple.
showMutantSAsTuple :: [String] -> MutantS -> String
showMutantSAsTuple ns (Tuple ms) = showTuple $ zipWith show1 (ns +- defaultFunctionNames) ms
  where show1 n  (Unmutated _) = n
        show1 n  (Function bs) = showLambda (fvnames n) bs
        show1 _  m             = showMutantS m
showMutantSAsTuple ns m = showMutantSAsTuple ns (Tuple [m])

-- Show top-level (maybe tuple) named 'MutantS' as a bindings.
-- In general, you want to 'flatten' the 'MutantS' before applying this
-- function.
showMutantSBindings :: Bool -> [String] -> MutantS -> String
showMutantSBindings new ns (Tuple ms) = concatMap (uncurry show1)
                                  $ zip (ns ++ defaultFunctionNames) ms
  where show1 _ (Unmutated s) = ""
        show1 _ (Function []) = ""
        show1 n (Function bs) = showBindings new (fvnames n) bs
        show1 n m             = let fn = head $ fvnames n
                                    fn' | new = prime fn
                                        | otherwise = fn
                                in (apply fn' [] ++ " = ")
                          `beside` showMutantS m
showMutantSBindings new ns m = showMutantSBindings new ns (Tuple [m])


-- | Given a list with the function and variable names and a list of bindings,
-- show a function as a case expression enclosed in a lambda.
showLambda :: [String] -> [([String],MutantS)] -> String
showLambda []    [] = "undefined {- (err?) unmutated -}"
showLambda (n:_) [] = apply n []
showLambda _ [([],m)]   = showMutantS m
showLambda _ (([],_):_) = "undefined {- (err?) ambiguous value -}"
showLambda ns bs = (("\\" ++ unwords bound ++ " -> ") `beside`)
                 $ "case " ++ showTuple bound ++ " of\n"
                ++ "  " `beside` cases
  where
    cases = concatMap (\(as,r) -> (showTuple as ++ " -> ") `beside` showResult r) bs
         ++ "_ -> " ++ apply fn bound
    showResult (Function bs') = showLambda (apply fn bound:unbound) bs'
    showResult m              = showMutantS m
    unbound  = drop (length bound) vns
    bound    = zipWith const vns (fst $ head bs)
    (fn:vns) = ns +- defaultNames

-- | Given a list with the function and variable names and a list of bindings,
-- show function binding declarations.
--
-- The 'new' boolean argument indicates whether if the function should be shown
-- as a new definition.
showBindings :: Bool -> [String] -> [([String],MutantS)] -> String
showBindings new ns bs =
  table " " $ (uncurry showBind `map` bs)
           ++ [words (apply fn' bound) ++ ["=", apply fn bound] | new]
  where
    showBind [a1,a2] r | isInfix fn' = [a1, fn', a2,   "=", showMutantS r]
    showBind as r                    = [fn'] ++ as ++ ["=", showMutantS r]
    fn' | new = prime fn
        | otherwise = fn
    bound    = zipWith const vns (fst $ head bs)
    (fn:vns) = ns +- defaultNames


-- | Separate function from variable names in a simple Haskell expr.
--
-- > fvarnames "f x y" == ["f","x","y"]
-- > fvarnames "aa bb cc dd" == ["aa","bb","cc","dd"]
--
-- When there are three lexemes, the function checks for a potential infix
-- operator in the middle.
--
-- > fvarnames "x + y" == ["(+)","x","y"]
-- 
-- This function always returns a "head"
--
-- > fvarnames "" == ["f"]
fvnames :: String -> [String]
fvnames = fvns' . words
  where fvns' :: [String] -> [String]
        fvns' [a,o,b] | isInfix o = o:[a,b]
        fvns' []      = defaultNames
        fvns' fvs     = fvs

-- | Apply a function ('String') to a list of variables ('[String]').
--
-- For the sake of clarity, in the following examples, double-quotes are omitted:
-- > apply f       == f
-- > apply f x     == f x
-- > apply f x y   == f x y
-- > apply (+)     == (+)
-- > apply (+) x   == (+) x
-- > apply (+) x y == (+) x y
-- > apply +       == (+)
-- > apply + x     == (+) x
-- > apply + x y   == (x + y)
-- > apply + x y z == (+) x y z
apply :: String -> [String] -> String
apply f [x,y] | isInfix f = unwords [x,f,y]
apply f xs = if isInfix f
               then unwords (toPrefix f:xs)
               else unwords (f:xs)

-- | Check if a function / operator is infix
--
-- > isInfix "foo"   == False
-- > isInfix "(+)"   == False
-- > isInfix "`foo`" == True
-- > isInfix "+"     == True
isInfix :: String -> Bool
isInfix (c:cs) = c /= '(' && not (isLetter c)

-- | Transform an infix operator into an infix function:
--
-- > toPrefix "`foo`" == "foo"
-- > toPrefix "+"     == "(+)"
toPrefix :: String -> String
toPrefix ('`':cs) = init cs
toPrefix cs = '(':cs ++ ")"

-- Primeify the name of a function by appending prime @'@ to functions and
-- minus @-@ to operators.
--
-- > prime "(+)"   == "(+-)"
-- > prime "foo"   == "foo'"
-- > prime "`foo`" == "`foo'`"
-- > prime "*"     == "*-
prime :: String -> String
prime ('`':cs) = '`':init cs ++ "'`" -- `foo` to `foo'`
prime ('(':cs) = '(':init cs ++ "-)" -- (+) to (+-)
prime cs | isInfix cs = cs ++ "-"    -- + to +-
         | otherwise  = cs ++ "'"    -- foo to foo'


mapFst :: (a->b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a->b) -> (c,a) -> (c,b)
mapSnd f (x,y) = (x,f y)

-- | @xs +- ys@ superimposes @xs@ over @ys@.
--
-- [1,2,3] +- [0,0,0,0,0,0,0] == [1,2,3,0,0,0,0]
-- [x,y,z] +- [a,b,c,d,e,f,g] == [x,y,z,d,e,f,g]
-- "asdf" +- "this is a test" == "asdf is a test"
(+-) :: Eq a => [a] -> [a] -> [a]
xs +- ys = xs ++ drop (length xs) ys


-- Instances of ShowMutable for up to 6-tuples are given here:

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d)
      => ShowMutable (a,b,c,d) where
  mutantS (f,g,h,i) (f',g',h',i') = Tuple [ mutantS f f'
                                          , mutantS g g'
                                          , mutantS h h'
                                          , mutantS i i' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c,
          ShowMutable d, ShowMutable e)
      => ShowMutable (a,b,c,d,e) where
  mutantS (f,g,h,i,j) (f',g',h',i',j') = Tuple [ mutantS f f'
                                               , mutantS g g'
                                               , mutantS h h'
                                               , mutantS i i'
                                               , mutantS j j' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c,
          ShowMutable d, ShowMutable e, ShowMutable f)
      => ShowMutable (a,b,c,d,e,f) where
  mutantS (f,g,h,i,j,k) (f',g',h',i',j',k') = Tuple [ mutantS f f'
                                                    , mutantS g g'
                                                    , mutantS h h'
                                                    , mutantS i i'
                                                    , mutantS j j'
                                                    , mutantS k k' ]

mutantSTuple :: [MutantS] -> MutantS
mutantSTuple = Tuple
