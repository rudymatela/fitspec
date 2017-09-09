{-# LANGUAGE TemplateHaskell #-}
import Test.FitSpec
import Test.LeanCheck.Tiers
import Algebra.Graph

-- TODO: The three following functions should be ported to LeanCheck soon.
--       So, we'll not need to declare them here.
discardLaterT :: (a -> a -> Bool) -> [[a]] -> [[a]]
discardLaterT d []           = []
discardLaterT d ([]:xss)     = [] : discardLaterT d xss
discardLaterT d ((x:xs):xss) = [[x]]
                            \/ discardLaterT d (discardT (`d` x) (xs:xss))

discardT :: (a -> Bool) -> [[a]] -> [[a]]
discardT p = filterT (not . p)

nubT :: Ord a => [[a]] -> [[a]]
nubT = discardLaterT (==)

instance (Listable a, Ord a) => Listable (Graph a) where
  tiers = nubT
        $ cons0 Empty
       \/ cons1 Vertex
       \/ cons2 Overlay
       \/ cons2 Connect

deriveMutableE [''Ord] ''Graph

instance Ord a => Ord (Graph a) where
  (<=) = isSubgraphOf

type A = Int

properties :: ( Graph A -> Graph A -> Graph A
              , Graph A -> Graph A -> Graph A )
           -> [Property]
properties (plus,times) =
  [ property $ \x y -> x + y == y + x
  , property $ \x y z -> x + (y + z) == (x + y) + z
  , property $ \x y z -> x * (y * z) == (x * y) * z
  , property $ \x y z -> x * (y * z) == (x * y) + (x * z) + (y * z)
  ]
  where
  (+) = plus
  (*) = times
  infixl 6 +
  infixl 7 *

main :: IO ()
main = do
  args <- getArgsWith
    args { names = ["x + y", "x * y"]
         , nMutants = 1000
         , nTests   = 1000
         , timeout  = 0
         }
  reportWithExtra
    [ ((*),(+))
    , ((*),(*))
    , ((+),(+)) ]
    args
    ((+),(*))
    properties
