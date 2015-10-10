module Mutate.Show
  ( ShowMutable (..)
  , showShowTree -- TODO: hide this export
  , showMutantEq
  )
where

import PPP
import Utils (errorToNothing)
import Test.Check
import Data.Maybe (mapMaybe)
import Control.Monad (join)
import Data.List (intercalate)

-- | Default function name, when none given
defFn :: String
defFn  = "fn"

-- | Default variable names, when none given
defVns :: [String]
defVns = ["x","y","z","w"] ++ map (++"'") defVns

(+-+) :: String -> String -> String
cs +-+ ds = cs ++ " " ++ ds
infixr 5 +-+

data ShowTree = Val String -- Change this to Vals [String]?
              | Bns [(String,ShowTree)]
  deriving (Show, Eq) -- Derivation only needed for debug and tests

showShowTrees' :: [ShowTree] -> String
showShowTrees' = concatMap showShowTree'

showShowTrees :: [(String,[String])] -> [ShowTree] -> String
showShowTrees ns = concat . zipWith showShowTree ns

-- Use default function and variable names
showShowTree' :: ShowTree -> String
showShowTree' = showShowTree (defFn,defVns)

showShowTree :: (String,[String]) -> ShowTree -> String
showShowTree (fn,[])     t = showShowTree (fn,defVns) t
showShowTree (fn,vn:vns) t =
  case t of
    Val s  -> s
    Bns bs -> (("\\" ++ vn ++ " -> ") `beside`)
            $ "case " ++ vn ++ " of\n"
           ++ "  " `beside` (concatMap showb bs ++ "_ -> " ++ fn +-+ vn ++ "\n")
  where showb (x,xt) = x `beside` " -> " `beside` showShowTree (fn +-+ vn,vns) xt


class ShowMutable a where
  showMutant :: a -> a -> String
  showMutant = showMutantN $ repeat (defFn,defVns)
  showMutantN :: [(String,[String])] -> a -> a -> String
  showMutantN _ = showMutant
  showTreeMutant :: a -> a -> [ShowTree]
  showTreeMutant f f' = case showMutant f f' of
                          "" -> []
                          s  -> [Val s]

showMutantEq :: (Show a, Eq a) => a -> a -> String
showMutantEq x x' = if x == x'
                      then ""
                      else show x'

instance ShowMutable ()   where showMutant = showMutantEq
instance ShowMutable Int  where showMutant = showMutantEq
instance ShowMutable Char where showMutant = showMutantEq
instance ShowMutable Bool where showMutant = showMutantEq
instance (Eq a, Show a) => ShowMutable [a]       where showMutant = showMutantEq
instance (Eq a, Show a) => ShowMutable (Maybe a) where showMutant = showMutantEq

instance (Listable a, Show a, ShowMutable b) => ShowMutable (a->b) where
  showMutantN []       f f' = showMutant f f'
  showMutantN (fvns:_) f f' =
    case showTreeMutant f f' of
      []  -> fst fvns ++ "\n"
      [t] -> showShowTree fvns t
      _   -> error "The impossible happened" -- TODO: Improve error message
  showTreeMutant f f' = case bindings of
                          [] -> []
                          _  -> [Bns bindings]
    where bindings = take 10
                   . mapMaybe bindingFor
                   . take 100
                   $ list
          bindingFor x = case errorToNothing $ showTreeMutant (f x) (f' x) of
                           Nothing  -> Nothing
                           Just []  -> Nothing
                           Just [y] -> Just (show x,y)
                           Just ys  -> Just (show x,Val $ showShowTrees' ys)

instance (ShowMutable a, ShowMutable b) => ShowMutable (a,b) where
  showMutantN [] p p' = showMutant p p'
  showMutantN ns (f,g) (f',g') = showMutantN ns f f' ++ showMutantN (tail ns) g g'

instance (ShowMutable a, ShowMutable b, ShowMutable c) => ShowMutable (a,b,c) where
  showMutantN [] p p' = showMutant p p'
  showMutantN ns (f,g,h) (f',g',h') = showMutantN ns f f' ++ showMutantN (tail ns) (g,h) (g',h')

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d) => ShowMutable (a,b,c,d) where
  showMutantN [] p p' = showMutant p p'
  showMutantN ns (f,g,h,i) (f',g',h',i') = showMutantN ns f f' ++ showMutantN (tail ns) (g,h,i) (g',h',i')
