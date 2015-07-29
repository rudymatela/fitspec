-- Enumeration of function mutations
module Mutation where

import Test.Check
import Test.Check.Function
import Data.List (intercalate)
import Data.Maybe
import Table
import CatchEvaluation
import Utils (spread,errorToNothing)

type Mutation a b = [(a,b)]

canonicalMutation :: Eq b => (a -> b) -> [(a, b)] -> Bool
-- This simple version on the line below
-- is one that does not deal with partially undefined functions.
-- canonicalMutation f = all (\(a,r) -> f a /= r)
canonicalMutation f = all different
  where
    -- the errorToNothing here deals partial functions (error/undefined)
    -- We define that mutating undefined values is noncanonical
    different (a,r) = case errorToNothing $ f a of
                        Just r' -> r' /= r
                        Nothing -> True -- for our purposes,
                                        -- undefined is equal to anything


-- The first mutant returned by szMutants and mutants is the actual function
-- without mutation.
szMutations :: (Eq b, Listable a, Listable b)
            => (a -> b) -> [[Mutation a b]]
szMutations f = lsfilter (canonicalMutation f)
              $ lsPartialFunctions listing listing

szMutations2 :: ( Eq b, Eq d
                , Listable a, Listable b
                , Listable c, Listable d )
             => (a -> b) -> (c -> d)
             -> [[(Mutation a b, Mutation c d)]]
szMutations2 f g = szMutations f `lsProduct` szMutations g

szMutations3 :: ( Eq b, Eq d, Eq f
                , Listable a, Listable b
                , Listable c, Listable d
                , Listable e, Listable f )
             => (a -> b) -> (c -> d) -> (e -> f)
             -> [[(Mutation a b, Mutation c d, Mutation e f)]]
szMutations3 f g h = szMutations f `lsProduct'` szMutations2 g h
  where lsProduct' = lsProductWith (\f' (g',h') -> (f',g',h'))


mutations :: (Eq b, Listable a, Listable b)
          => (a -> b)
          -> [Mutation a b]
mutations = concat . szMutations

mutations2 :: ( Eq b, Eq d
              , Listable a, Listable b
              , Listable c, Listable d )
           => (a -> b) -> (c -> d)
           -> [(Mutation a b, Mutation c d)]
mutations2 f g = concat (szMutations2 f g)

mutations3 :: ( Eq b, Eq d, Eq f
              , Listable a, Listable b
              , Listable c, Listable d
              , Listable e, Listable f )
           => (a -> b) -> (c -> d) -> (e -> f)
           -> [(Mutation a b, Mutation c d, Mutation e f)]
mutations3 f g h = concat (szMutations3 f g h)

ifNothing :: (a -> b) -> (a -> Maybe b) -> a -> b
ifNothing f g x = fromMaybe (f x) (g x)

makeMutant :: Eq a
           => (a -> b) -> Mutation a b
           -> a -> b
makeMutant f = ifNothing f . bindingsToFunction'

makeMutantTraps :: Eq a
                => (a -> b) -> Mutation a b
                -> [a -> b]
makeMutantTraps f = map (makeMutant f) . makeBindingTraps

makeMutantTraps2 :: (Eq a, Eq c)
                 => (a->b) -> (c->d)
                 -> Mutation a b -> Mutation c d
                 -> [(a->b, c->d)]
makeMutantTraps2 f g bs cs = map (\(bs',cs') -> (makeMutant f bs', makeMutant g cs'))
                                 (makeBindingTraps2 bs cs)

makeMutantTraps3 :: (Eq a, Eq c, Eq e)
                 => (a->b) -> (c->d) -> (e->f)
                 -> Mutation a b -> Mutation c d -> Mutation e f
                 -> [(a->b, c->d, e->f)]
makeMutantTraps3 f g h bs cs ds = map (\(bs',cs',ds') -> (makeMutant f bs', makeMutant g cs', makeMutant h ds'))
                                      (makeBindingTraps3 bs cs ds)

makeBindingTraps :: Eq a => Mutation a b -> [Mutation a b]
makeBindingTraps = spread (fmap throwE)

makeBindingTraps2 :: (Eq a, Eq c) => Mutation a b -> Mutation c d -> [(Mutation a b,Mutation c d)]
makeBindingTraps2 bs cs = map (\bs' -> (bs',cs)) (makeBindingTraps bs)
                       ++ map (\cs' -> (bs,cs')) (makeBindingTraps cs)

makeBindingTraps3 :: (Eq a, Eq c, Eq e)
                  => Mutation a b -> Mutation c d -> Mutation e f
                  -> [(Mutation a b, Mutation c d, Mutation e f)]
makeBindingTraps3 bs cs ds = map (\bs' -> (bs',cs,ds)) (makeBindingTraps bs)
                          ++ map (\(cs',ds') -> (bs,cs',ds')) (makeBindingTraps2 cs ds)

showMutant_ :: (Show a, Show b)
            => String -> String -> Mutation a b -> String
showMutant_ funName varName bindings =
  case bindings of
    [] -> "\\" ++ varName ++ " -> " ++ funName ++ " " ++ varName ++ "\n"
    _  -> columns " " [ "\\" ++ varName ++ " -> case " ++ varName ++ " of ", caseExp ]
  where caseExp = table " -> "
                . (++ if length bindings == 10 -- probably infinite ;-P  FIXME
                        then [["...", "..."]]
                        else [["_", funName ++ " " ++ varName]])
                . map (\(a,r) -> [show a, show r])
                $ bindings

