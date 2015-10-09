module Mutate.Show
  ( ShowMutable (..)
  )
where

import PPP
import Utils (errorToNothing)
import Test.Check

class ShowMutable a where
  showMutant :: a -> a -> String
  showMutant = showMutantN "function" "x"
  showMutantN :: String -> String -> a -> a -> String
  showMutantN _ _ = showMutant
  -- Pairs represent names and parameters
  showMutant' :: [(String,[String])] -> a -> a -> String
  showMutant' = undefined

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

{- TODO: Possible future intermediate representation before printing -}
data ShowMutant = Null
                | Actual
data Actual = Value String
            | Bindings [(String,Actual)]

instance (Listable a, Show a, ShowMutable b) => ShowMutable (a->b) where
  showMutant f f' = case bindings of
                      [] -> ""
                      _  -> beside "\\x -> "
                          $ "case x of\n" ++
                            "  " `beside` (concat bindings ++ "_ -> original\n")
    where bindings = take 10
                   . filter (not . null)
                   . map bindingFor
                   . take 100
                   $ list
          bindingFor x = case errorToNothing $ showMutant (f x) (f' x) of
                           Nothing -> ""
                           Just "" -> ""
                           Just s  -> show x `beside` " -> " `beside` s

instance (ShowMutable a, ShowMutable b) => ShowMutable (a,b) where
  showMutant (f,g) (f',g') = showMutant f f' ++ showMutant g g'

instance (ShowMutable a, ShowMutable b, ShowMutable c) => ShowMutable (a,b,c) where
  showMutant (f,g,h) (f',g',h') = showMutant f f' ++ showMutant g g' ++ showMutant h h'

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d) => ShowMutable (a,b,c,d) where
  showMutant (f,g,h,i) (f',g',h',i') = showMutant f f' ++ showMutant g g' ++ showMutant h h' ++ showMutant i i'
