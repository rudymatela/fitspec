-- | Show mutant variations
module FitSpec.ShowMutable
  ( ShowMutable (..)
  , mutantSEq
  , showMutant
  , showMutantN
  , showMutantNested
  , showMutantBind
  , MutantS ()
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
import Data.List (intercalate,tails)
import Data.Char (isLetter)

-- | Structure of a mutant.
data MutantS = Unmutated String
             | Atom String
             | Tuple [MutantS]
             | Function [([String],MutantS)]
  deriving Show

-- | Default function name, when none given
defFn :: String
defFn  = head defFns

defFns :: [String]
defFns = ["f","g","h"] ++ map (++"'") defFns

-- | Default variable names, when none given
defVns :: [String]
defVns = ["x","y","z","w"] ++ map (++"'") defVns

isUnmutated (Unmutated _) = True
isUnmutated (Tuple ms)    = all isUnmutated ms
isUnmutated (Function bs) = all (isUnmutated . snd) bs
isUnmutated _             = False

isFunction (Function _) = True
isFunction _            = False

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

-- | Show a nameless mutant
-- Functions (can but) should not be shown using this.
showMutantS :: MutantS -> String
showMutantS (Unmutated s) = s
showMutantS (Atom s)      = s
showMutantS (Tuple ms)    = showTuple $ map showMutantS ms
showMutantS (Function bs) = showLambda ["??"] bs

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
    (fn:vns) = ns +- (defFn:defVns)

-- | Top-level (maybe tuple) named mutant.
showMutantSTuple :: [String] -> MutantS -> String
showMutantSTuple ns (Tuple ms) = showTuple $ zipWith show1 (ns ++ defFns) ms
  where show1 n  (Unmutated _) = n
        show1 n  (Function bs) = showLambda (fvnames n) bs
        show1 _  m             = showMutantS m
showMutantSTuple ns m          = showMutantSTuple ns (Tuple [m])

showMutantSBind :: [String] -> MutantS -> String
showMutantSBind ns (Tuple ms) = concatMap (uncurry show1) $ zip (ns++defFns) ms
  where show1 _ (Unmutated s) = ""
        show1 _ (Function []) = ""
        show1 n (Function bs) = showBindings (fvnames n) bs
        show1 n m             = ((prime (fname n) `apply` []) ++ " = ")
                       `beside` showMutantS m
showMutantSBind ns m          = showMutantSBind ns (Tuple [m])

-- | Given a list with the function and variable names and a list of bindings,
-- show function binding declarations.
showBindings :: [String] -> [([String],MutantS)] -> String
showBindings ns bs = table " "
                   $ (uncurry showBind `map` bs)
                  ++ [words (apply fn' bound) ++ ["=", apply fn bound]]
  where
    showBind [a1,a2] r | isInfix fn' = [a1, fn', a2,   "=", showMutantS r]
    showBind as r                    = [fn'] ++ as ++ ["=", showMutantS r]
    fn'      = prime fn
    bound    = zipWith const vns (fst $ head bs)
    (fn:vns) = ns +- (defFn:defVns)

showMutantN :: ShowMutable a => [String] -> a -> a -> String
showMutantN names f f' = showMutantSTuple names
                       $ flatten
                       $ mutantS f f'

showMutantBind :: ShowMutable a => [String] -> a -> a -> String
showMutantBind names f f' = showMutantSBind names
                          $ flatten
                          $ mutantS f f'

showMutantNested :: ShowMutable a => [String] -> a -> a -> String
showMutantNested names f f' = showMutantSTuple names
                            $ mutantS f f'

showMutant :: ShowMutable a => a -> a -> String
showMutant = showMutantN []

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
        fvns' []      = [defFn]
        fvns' fvs     = fvs

fname :: String -> String
fname = head . fvnames

-- TODO: Check if 'f' is intended to be used as an infix operator and operate accordingly
-- even if 'f' is (10 +).  Transform into (10 + 2).
--
-- For the sake of clarity, in the following examples, double-quotes are ommited:
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

isInfix :: String -> Bool
isInfix (c:cs) = c /= '(' && not (isLetter c)

toPrefix :: String -> String
toPrefix ('`':cs) = init cs
toPrefix cs = '(':cs ++ ")"

-- Primeify the name of a function
prime :: String -> String
prime ('`':cs) = '`':init cs ++ "'`" -- `foo` to `foo'`
prime ('(':cs) = '(':init cs ++ "-)" -- (+) to (+-)
prime cs | isInfix cs = cs ++ "-"    -- + to +-
         | otherwise  = cs ++ "'"    -- foo to foo'


class ShowMutable a where
  mutantS :: a -> a -> MutantS

mutantSEq :: (Eq a, Show a)
           => a -> a -> MutantS
mutantSEq x x' = if x == x'
                    then Unmutated $ show x
                    else Atom      $ show x'

instance ShowMutable ()   where mutantS = mutantSEq
instance ShowMutable Int  where mutantS = mutantSEq
instance ShowMutable Char where mutantS = mutantSEq
instance ShowMutable Bool where mutantS = mutantSEq
instance (Eq a, Show a) => ShowMutable [a]       where mutantS = mutantSEq
instance (Eq a, Show a) => ShowMutable (Maybe a) where mutantS = mutantSEq


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

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g)
      => ShowMutable (a,b,c,d,e,f,g) where
  mutantS (f,g,h,i,j,k,l) (f',g',h',i',j',k',l') = Tuple
                                                    [ mutantS f f'
                                                    , mutantS g g'
                                                    , mutantS h h'
                                                    , mutantS i i'
                                                    , mutantS j j'
                                                    , mutantS k k'
                                                    , mutantS l l' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h)
      => ShowMutable (a,b,c,d,e,f,g,h) where
  mutantS (f,g,h,i,j,k,l,m) (f',g',h',i',j',k',l',m') = Tuple
                                                      [ mutantS f f'
                                                      , mutantS g g'
                                                      , mutantS h h'
                                                      , mutantS i i'
                                                      , mutantS j j'
                                                      , mutantS k k'
                                                      , mutantS l l'
                                                      , mutantS m m' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i)
      => ShowMutable (a,b,c,d,e,f,g,h,i) where
  mutantS (f,g,h,i,j,k,l,m,n) (f',g',h',i',j',k',l',m',n') = Tuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j)
      => ShowMutable (a,b,c,d,e,f,h,g,i,j) where
  mutantS (f,g,h,i,j,k,l,m,n,o) (f',g',h',i',j',k',l',m',n',o') = Tuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n'
    , mutantS o o' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j, ShowMutable k)
      => ShowMutable (a,b,c,d,e,f,g,h,i,j,k) where
  mutantS (f,g,h,i,j,k,l,m,n,o,p) (f',g',h',i',j',k',l',m',n',o',p') = Tuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n'
    , mutantS o o'
    , mutantS p p' ]

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d,
          ShowMutable e, ShowMutable f, ShowMutable g, ShowMutable h,
          ShowMutable i, ShowMutable j, ShowMutable k, ShowMutable l)
      => ShowMutable (a,b,c,d,e,f,g,h,i,j,k,l) where
  mutantS (f,g,h,i,j,k,l,m,n,o,p,q) (f',g',h',i',j',k',l',m',n',o',p',q') = Tuple
    [ mutantS f f'
    , mutantS g g'
    , mutantS h h'
    , mutantS i i'
    , mutantS j j'
    , mutantS k k'
    , mutantS l l'
    , mutantS m m'
    , mutantS n n'
    , mutantS o o'
    , mutantS p p'
    , mutantS q q' ]

-- Like tails, but returns an infinite list of empty lists at the end
drops :: [a] -> [[a]]
drops xs = tails xs ++ repeat []

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
