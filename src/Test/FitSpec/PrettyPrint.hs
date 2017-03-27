-- |
-- Module      : Test.FitSpec.PrettyPrint
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- A very simple pretty printing library used to generate 'Test.FitSpec' reports.
module Test.FitSpec.PrettyPrint
  ( beside
  , above
  , showTuple
  , table
  , columns
  , showQuantity
  , showEach
  , headToUpper
  )
where
-- TODO: Fix somewhat inefficient implementations, i.e.: heavy use of '(++)'.

import Data.List (intercalate,transpose,isSuffixOf)
import Data.Char (toUpper)

showQuantity :: Int -> String -> String
showQuantity 1 what = "1 " ++ what
showQuantity n what = show n ++ " " ++ pluralize what

showEach :: Show a => String -> [a] -> String
showEach what [x] = what ++ " " ++ show x
showEach what xs  = "each of " ++ pluralize what ++ " "
                 ++ intercalate ", " (map show $ init xs)
                 ++ " and "
                 ++ show (last xs)

-- | Pluralizes a word.
-- Is not comprehensive (and may never be),
-- add missing cases as they are found.
--
-- > pluralize "test case" == "test cases"
-- > pluralize "property"  == "properties"
pluralize :: String -> String
pluralize s | s `ew` "se" = s ++ "s"
            | s `ew` "n"  = s ++ "s"
            | s `ew` "y"  = init s ++ "ies"
            | otherwise   = s
  where ew = flip isSuffixOf


-- | Appends two Strings side by side, line by line
--
-- > beside ["asdf\nqw\n","zxvc\nas"] ==
-- >  "asdfzxvc\n\
-- >  \qw  as\n"
beside :: String -> String -> String
beside cs ds = unlines $ zipWith (++) (normalize ' ' css) dss
  where [css,dss] = normalize "" [lines cs,lines ds]

-- | Append two Strings on top of each other, adding line breaks *when needed*.
above :: String -> String -> String
above cs ds = if last cs == '\n' || head ds == '\n'
                then cs ++ ds
                else cs ++ '\n':ds

-- | Show elements of a list as a tuple.  If there are multiple lines in any of
--   the strings, tuple is printed multiline.
--
-- > showTuple ["asdf\nqwer\n","zxvc\nasdf\n"] ==
-- >   "( asdf\n\
-- >   \  qwer\n\
-- >   \, zxvc\n\
-- >   \  asdf )\n"
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

-- | Formats a table using a given separator.
--
-- > table "  " [ ["asdf", "qwer",     "zxvc\nzxvc"]
-- >            , ["0",    "1",        "2"]
-- >            , ["123",  "456\n789", "3"] ] ==
-- >   "asdf  qwer  zxvc\n\
-- >   \            zxvc\n\
-- >   \0     1     2\n\
-- >   \123   456   3\n\
-- >   \      789\n\"
table :: String -> [[String]] -> String
table s []  = ""
table s sss = unlines
            . map (removeTrailing ' ')
            . map (intercalate s)
            . transpose
            . map (normalize ' ')
            . foldr1 (zipWith (++))
            . map (normalize "" . map lines)
            . normalize ""
            $ sss

-- | Given a separator, format strings in columns
--
-- > columns " | " ["asdf", "qw\nzxcv", "as\ndf"] ==
-- >   "asdf | qw   | as\n\
-- >   \     | zxcv | df\n"
columns :: String -> [String] -> String
columns s = unlines
          . map (removeTrailing ' ')
          . map (intercalate s)
          . transpose
          . map (normalize ' ')
          . normalize ""
          . map lines

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
normalize x xs = map (x `fit` maxLength xs) xs

-- | Given a list of lists returns the maximum length
maxLength :: [[a]] -> Int
maxLength = maximum . (0:) . map length

removeTrailing :: Eq a => a -> [a] -> [a]
removeTrailing x = reverse
                 . dropWhile (==x)
                 . reverse

headToUpper :: [Char] -> [Char]
headToUpper [] = []
headToUpper (c:cs) = toUpper c : cs
