-- Poor man's pretty printing library

module PPP
  ( beside
  )
where

import Data.List (intercalate)

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
      ++ concatMap (", " `beside`) ss
      ++ ") "
    else "(" ++ intercalate "," (s:ss) ++ ")"
