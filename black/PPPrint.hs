-- | Poor man's pretty printing library
module PPPrint
  ( beside
  , showTuple
  , table
  , columns
  )
where

import Data.List (intercalate,transpose)

-- | Fits a list to a certain width by appending a certain value
--
-- > fit ' ' 6 "str" == "str   "
--
-- > fit 0 6 [1,2,3] == [1,2,3,0,0,0]
fit :: a -> Int -> [a] -> [a]
fit x n xs = xs ++ replicate (n - length xs) x

-- | normalize makes all list the same length by adding a value
--
-- > normalize ["asdf","qw","er"] == normalize ["asdf","qw  ","er  "]
normalize :: a -> [[a]] -> [[a]]
normalize x xs = map (fit x (maxlen xs)) xs
  where maxlen = maximum . map length

-- | Appends two Strings side by side, line by line
beside :: String -> String -> String
beside cs ds = unlines $ zipWith (++) (normalize ' ' css) dss
  where [css,dss] = normalize "" [lines cs,lines ds]


-- | Show elements of a list as a tuple.  If there are multiple lines in any of
--   the strings, tuple is printed multiline.
--
-- > showTuple ["asdf\nqwer\n","zxvc\nasdf\n"] ==
-- >   "( asdf
-- >      qwer
-- >    , zxvc
-- >      asdf )"
--
-- > showTuple ["asdf","qwer"] == "(asdf,qwer)"
showTuple :: [String] -> String
showTuple []  = ""
showTuple [s] = s
showTuple (s:ss) =
  if any ('\n' `elem`) (s:ss)
    then "( " `beside` s
      ++ init (concatMap (", " `beside`) ss)
      ++ " )\n"
    else "(" ++ intercalate "," (s:ss) ++ ")"

-- TODO: Remove trailing spaces from the resulting table string.

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
