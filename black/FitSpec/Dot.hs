module FitSpec.Dot where

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

isObvious :: Eq i => [[i]] -> [[i]] -> Bool
isObvious as bs = or [ a `contains` b
                     | a <- as
                     , b <- bs ]

attachObviousness :: Eq i => [([[i]], [[i]])] -> [([[i]],[[i]],Bool)]
attachObviousness = map attachObviousness'
  where attachObviousness' (as,bs) = (as,bs,isObvious as bs)


-- | Given a list of relations, generate a graphviz graph containing those relations.
--   Generate a dotfile from implications between groups.
genDotfileFromGI :: Show i
                 => [([[i]],[[i]],Bool)]
                 -> String
genDotfileFromGI = (\s -> "digraph G {\n" ++ s ++ "}\n")
                 . unlines
                 . map showEntry
  where showG = unwords . map show
        showEntry (iss,jss,p) = "\"" ++ showG iss ++ "\" -> \""
                                     ++ showG jss ++ "\""
                             ++ if p
                                  then " [ color = grey ]"
                                  else ""

-- | Equivalent to 'getResults' but returns a dotfile
getDotfile :: (Mutable a)
           => [a]
           -> Int -> a -> (a -> [Bool])
           -> String
getDotfile ems n f = genDotfileFromGI
                   . attachObviousness
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
