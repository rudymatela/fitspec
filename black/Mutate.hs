-- Enumeration of function mutations
module Mutate where

import Test.Check
import Test.Check.Function
import Data.List (intercalate)
import Data.Maybe
import Table

-- The first mutant returned by szMutants and mutants is the actual function
-- without mutation.
class Mutable a where
  szMutants :: a -> [[a]]
  mutants :: a -> [a]
  szMutants = map (:[]) . mutants
  mutants = concat . szMutants

-- Here canonicalMutation instead of proper mutation is being used: this is
-- necessary when making the product of several mutations (in a tuple of
-- functions there is need to mutate a single element).
instance (Eq a, Eq b, Listable a, Listable b) => Mutable (a -> b) where
  szMutants f = lsmap (ifNothing f . bindingsToFunction')
              $ map (filter $ canonicalMutation f)
              $ lsPartialFunctions listing listing

instance (Mutable a, Mutable b) => Mutable (a,b) where
  szMutants (f,g) = szMutants f `lsProduct` szMutants g

instance (Mutable a, Mutable b, Mutable c) => Mutable (a,b,c) where
  szMutants (f,g,h) = lsProductWith (\f' (g',h') -> (f',g',h')) (szMutants f) (szMutants (g,h))

instance (Mutable a, Mutable b, Mutable c, Mutable d) => Mutable (a,b,c,d) where
  szMutants (f,g,h,i) = lsProductWith (\f' (g',h',i') -> (f',g',h',i')) (szMutants f) (szMutants (g,h,i))

ifNothing :: (a -> b) -> (a -> Maybe b) -> a -> b
ifNothing f g x = fromMaybe (f x) (g x)

-- NOTE: use tail to exclude "id" mutant
-- if a==b, this function will enumerate several id mutants, starting with the second:
-- [], [(0,0)], <--- here, then:   [(0,1)], [(0,0),(1,0)], [(0,2)], ...
-- [(0,1)] and [(0,0) and (1,0)] are the same funct.
-- use a specialized mutator??  I think it is not necessary
repeatedMutants :: (Eq a, Listable a, Listable b) => (a -> b) -> [a -> b]
repeatedMutants f = map (ifNothing f . bindingsToFunction') (partialFunctions listing listing)

strictMutants :: (Eq a, Eq b, Listable a, Listable b) => (a->b) -> [a->b]
strictMutants f = map (ifNothing f . bindingsToFunction') (mutantBindings f)

-- TODO: mutantBindings can be better. Move into partialFunctions? custom enum?
mutantBindings :: (Eq a, Eq b, Listable a, Listable b) => (a->b) -> [[(a,b)]]
mutantBindings f = filter (properMutation f) (partialFunctions listing listing)

properMutation :: Eq b => (a -> b) -> [(a, b)] -> Bool
properMutation f [] = False
properMutation f bs = all (\(a,r) -> f a /= r) bs

canonicalMutation :: Eq b => (a -> b) -> [(a, b)] -> Bool
canonicalMutation f = all (\(a,r) -> f a /= r)

nePartialFunctions :: [[a]] -> [[b]] -> [[(a,b)]]
nePartialFunctions ass = tail . partialFunctions ass

-- TODO: make this prettier
showMutant_ :: (Listable a, Show a, Show b, Eq b)
            => String -> String -> (a -> b) -> (a -> b) -> String
showMutant_ funName varName f f' =
  case bindings of
    [] -> "\\" ++ varName ++ " -> " ++ funName ++ " " ++ varName ++ "\n"
    _  -> columns " " [ "\\" ++ varName ++ " -> case " ++ varName ++ " of ", caseExp ]
  where caseExp = table " -> "
                . (++ if length bindings == 10 -- probably infinite ;-P  FIXME
                        then [["...", "..."]]
                        else [["_", funName ++ " " ++ varName]])
                . map (\(a,r) -> [show a, show r])
                $ bindings
        bindings = take 10
                 . mapMaybe bindingFor
                 . take 200
                 $ list
        bindingFor a = if f a /= f' a
                         then Just (a,f' a)
                         else Nothing

class ShowMutable a where
  showMutant :: a -> a -> String
  showMutant = showMutantN "function" "x"
  showMutantN :: String -> String -> a -> a -> String
  showMutantN _ _ = showMutant

instance (Listable a, Show a, Show b, Eq b) => ShowMutable (a->b) where
  showMutantN = showMutant_

instance (ShowMutable a, ShowMutable b) => ShowMutable (a,b) where
  showMutant (f,g) (f',g') = showMutant f f' ++ showMutant g g'

instance (ShowMutable a, ShowMutable b, ShowMutable c) => ShowMutable (a,b,c) where
  showMutant (f,g,h) (f',g',h') = showMutant f f' ++ showMutant g g' ++ showMutant h h'

instance (ShowMutable a, ShowMutable b, ShowMutable c, ShowMutable d) => ShowMutable (a,b,c,d) where
  showMutant (f,g,h,i) (f',g',h',i') = showMutant f f' ++ showMutant g g' ++ showMutant h h' ++ showMutant i i'
