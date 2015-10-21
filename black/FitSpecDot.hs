module FitSpecDot where

import FitSpec
import Mutate
import Utils
import Data.List

relevantSI :: Eq i => [[i]] -> ([[i]],[i])
relevantSI iss = ( relevantPropertySets iss
                 , relevantImplications iss )

-- | Given a list of pairs of property groups and their implications,
--   return implications between groups (transitive cases are ommitted).
groupImplications :: Eq i => [([[i]], [i])] -> [([[i]],[[i]])]
groupImplications [] = []
groupImplications (n:ns) = [ (fst n, fst n')
                           | n' <- filterU (not ... implies)
                                 . filter (n `implies`)
                                 $ ns
                           ] ++ groupImplications ns
  where actual (iss,is) = foldr union [] iss `union` is
        n `implies` m = actual n `contains` actual m

-- | Given a list of relations, generate a graphviz graph containing those relations
genDotfile :: [(String,String)] -> String
genDotfile = (\s -> "digraph G {\n" ++ s ++ "}\n")
           . unlines
           . map (\(a,b) -> "\"" ++ a ++ "\" -> \"" ++ b ++ "\"")

-- | Generate a dotfile from implications between groups
genDotfileFromGI :: Show i
                  => [([[i]],[[i]])]
                  -> String
genDotfileFromGI = genDotfile . map showEntry
  where showEntry (iss,jss) = (showG iss,showG jss)
        showG = unwords . map show

-- | Equivalent to 'getResults' but returns a dotfile
getDotfile :: (Mutable a)
           => [a]
           -> Int -> a -> (a -> [Bool])
           -> String
getDotfile ems n f = genDotfileFromGI
                   . groupImplications
                   . map (relevantSI . fst)
                   . getRawResults ems n f

-- | Equivalent to report, but writes a dotfile to a file
writeDotfile :: (Mutable a)
             => String
             -> [a]
             -> Int -> a -> (a -> [Bool])
             -> IO ()
writeDotfile fn ems n f = writeFile fn . getDotfile ems n f

-- | Equivalent to report, but writes a dotfile to stdout
putDotfile :: (Mutable a)
           => [a]
           -> Int -> a -> (a -> [Bool])
           -> IO ()
putDotfile ems n f = putStr . getDotfile ems n f
