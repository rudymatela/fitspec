-- | FitSpec tries to find minimal and complete subsets of a specification via
--   black-box mutation testing.
--
-- NOTE:
--   * The API is likely to change in the near future.
--   * For now, everything is exported.
--
-- Example -- properties over not, 500 mutants, 1000 tests each:
--
-- > import Test.Check
-- > import MinProps
-- >
-- > pMap :: Int -> (Bool -> Bool) -> [Bool]
-- > pMap not' n =
-- >   [ holds n $ \p -> not' (not' p) == p
-- >   , holds n $ \p -> not' p /= p
-- >   ,                 not' True == False
-- >   ]
-- >
-- > report 500 not (pMap 1000)
--
-- FitSpec should report that the minimal specifications are either:
--
--   * 1 and 2
--   * 1 and 3
--   * 2 and 3

module FitSpec where

import Test.Check
import Test.Check.Utils
import Data.List
import Data.Ord
import Data.Monoid
import Data.Maybe (catMaybes, listToMaybe)
import Mutate
import Table
import Utils

-- | Extra arguments for 'getResultsWith' and 'reportWith'
data Args a = Args
            { extraMutants :: [a]
            , functionName :: String
            , variableName :: String
            , limitResults :: Maybe Int -- maximum number of results
            }

-- | Default arguments
args = Args { extraMutants = []
            , functionName = "function"
            , variableName = "x"
            , limitResults = Nothing    -- show everything
            }


-- | Return minimality and completeness results.  See 'report'.
getResults :: (Mutable a)
           => Int -> a -> (a -> [Bool])
           -> [(Int, [Int], Maybe a)]
getResults = getResultsWith args

-- | Return minimality and completeness results.  See 'reportWith'.
getResultsWith :: (Mutable a)
               => Args a
               -> Int -> a -> (a -> [Bool])
               -> [(Int, [Int], Maybe a)]
getResultsWith args nMuts f propMap = maybe id take (limitResults args)
                                    $ nSurvT pids propMap muts
  where pids = [1..(length (propMap f))]
        muts = take nMuts (tail $ mutants f)
            ++ extraMutants args

-- | Report minimality and completeness results.  Uses the standard
--   configuration (see 'args').  Needs the number of mutants to be generated,
--   a function to be mutated and a property map.
report :: (ShowMutable a, Mutable a)
       => Int -> a -> (a -> [Bool]) -> IO ()
report = reportWith args

-- | Same as 'report' but extra mutants and custom function names can be passed
--   via 'args'.
reportWith :: (ShowMutable a, Mutable a)
           => Args a
           -> Int
           -> a
           -> (a -> [Bool])
           -> IO ()
reportWith args nf f propMap = putStrLn
                             . table "   "
                             . map showResult
                             $ getResultsWith args nf f propMap
  where showResult (x,y,mm) = [ show x, show y, showM mm ]
        showM (Nothing) = ""
        showM (Just m)  = showMutantN (functionName args) (variableName args) f m ++ "\n"

-- | 'nSurv' @props fs@ returns the number of values that match
--   compositions of properties on the property map.
--
-- * @props@ should be a function from a value to a list of properties that
--   match that value (in the case of functions, functions that "survive" those
--   properties).
--
-- * @fs@ is a list of values to be mapped over by @props@
--
-- > length (nSurvivors props fs)  ==  2 ^ (length (props fs))
--
-- This function is otherwise unused in this file.  It is just a simpler
-- version of 'nSurvT' to serve as documentation.
nSurv :: (a -> [Bool]) -> [a] -> [Int]
nSurv props = map countTrue
            . transpose
            . map (compositions . props)
  where countTrue = length . filter id

-- | 'nSurvT' is the same as 'nSurv' but the number of surviving mutants is
--   tupled with the ids of properties and first survivor.
nSurvT :: Eq pid => [pid] -> (a -> [Bool]) -> [a] -> [(Int,[pid],Maybe a)]
nSurvT pids pmap = filterU relevant
                 . sortBy (comparing $ \(s,ids,_) -> (s, length ids))
                 . zipWith (\ids sms -> (length sms, ids, listToMaybe sms))
                           (subsets pids)
                 . map catMaybes
                 . transpose
                 . map (\func -> map (boolToMaybe func)
                                     (compositions (pmap func)))
  where (n,is,_) `relevant` (n',is',_) = not (n == n' && is `contained` is')

-- | 'compositions' @bs@ returns all compositions formed by taking values of @bs@
compositions :: [Bool] -> [Bool]
compositions = map and . subsets

mutateBy :: (b->a) -> [a->a] -> [b->a]
mutateBy f = map (. f)

mutateBySz :: (b->a) -> [[a->a]] -> [b->a]
mutateBySz f dfss = f `mutateBy` concat dfss
