module Table where

-- TODO: Remove trailing spaces from the resulting table string.

import Data.List

-- TODO: make this prettier (see Mutate.hs's mutant printing)
-- TODO: remove foldr1 use
table :: String -> [[String]] -> String
table s sss = unlines
            . map (intercalate s)
            . transpose
            . map (padl ' ')
            . foldr1 (zipWith (++))
            . map (padl "" . map lines)
            . padl ""
            $ sss

columns :: String -> [String] -> String
columns s ss = unlines
             . map (intercalate s)
             . transpose
             . map (padl ' ')
             . padl ""
             . map lines
             $ ss

pad :: Int -> a -> [a] -> [a]
pad 0 p xs     = xs
pad n p []     = replicate n p
pad n p (x:xs) = x:pad (n-1) p xs

padl :: a -> [[a]] -> [[a]]
padl p xss = map (pad (maxLength xss) p) xss

maxLength :: [[a]] -> Int
maxLength = maximum . (0:) . map length
