module FitSpecDot where

import FitSpec
import Mutate
import Utils
import Data.List

genDot' :: (Mutable a)
        => [a]
        -> Int -> a -> (a -> [Bool])
        -> [([[Int]], [Int])]
genDot' ems nms f = map (uncurry process)
                  . getRawResults ems nms f
  where process iss mms = ( filterRelevantPropertySets iss
                          , relevantImplications iss )
        relevantImplications iss = foldr union [] iss
                                \\ foldr union [] (filterRelevantPropertySets iss)

genDot'' :: [([[Int]], [Int])] -> [([[Int]],[[Int]])]
genDot'' [] = []
genDot'' (n:ns) = [ (fst n, fst n')
                  | n' <- filterU (not ... implies)
                        . filter (n `implies`)
                        $ ns
                  ] ++ genDot'' ns
  where actual (iss,is) = foldr union [] iss `union` is
        n `implies` m = actual n `contains` actual m

genDot''' :: [([[Int]],[[Int]])]
          -> String
genDot''' = unlines . map showEntry
  where showEntry (iss,jss) = "\""
                           ++ showG iss
                           ++ "\" -> \""
                           ++ showG jss
                           ++ "\""
        showG = unwords . map show

genDot :: (Mutable a)
       => [a]
       -> Int -> a -> (a -> [Bool])
       -> String
genDot em nm f = (\s -> "digraph G {\n" ++ s ++ "}\n")
               . genDot'''
               . genDot''
               . genDot' em nm f
