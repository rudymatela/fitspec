-- |
-- Module      : Test.FitSpec.Dot
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Experimental module to generate dotfiles (for graphviz) with implications
-- between property sub-sets.
module Test.FitSpec.Dot where

import Test.FitSpec
import Test.FitSpec.Engine
import Test.FitSpec.Utils
import Data.List

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
           -> Int -> Int -> a -> (a -> [Property])
           -> String
getDotfile ems m n f ps = genDotfileFromGI
                        . attachObviousness
                        . groupImplications
                        . map (\r -> (sets r, implied r))
                        $ getResultsExtra ems f ps m n

-- | Equivalent to report, but writes a dotfile to a file
writeDotfile :: (Mutable a)
             => String
             -> [a]
             -> Int -> Int -> a -> (a -> [Property])
             -> IO ()
writeDotfile fn ems m n f = writeFile fn . getDotfile ems m n f

-- | Equivalent to report, but writes a dotfile to stdout
putDotfile :: (Mutable a)
           => [a]
           -> Int -> Int -> a -> (a -> [Property])
           -> IO ()
putDotfile ems m n f = putStr . getDotfile ems m n f
