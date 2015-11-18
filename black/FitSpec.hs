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

module FitSpec
  ( Args(..)
  , args
  , report
  , reportWith
  , getRawResults
  , relevantImplications
  , relevantPropertySets
  , ShowMutable
  )
where

import Test.Check
import Test.Check.Utils
import Data.List
import Data.Ord
import Data.Monoid
import Data.Maybe (catMaybes, listToMaybe, isJust)
import Control.Monad (unless)
import Mutate
import Mutate.Show (ShowMutable)
import qualified Mutate.Show (showMutantN)
import Utils
import PPPrint

-- | Extra arguments / configuration for 'reportWith'.
--   See 'args' for default values.
data Args a = Args
  { extraMutants :: [a]   -- ^ extra mutants to try to kill alongside mutations
  , callNames :: [String] -- ^ function call templates: @["foo x y","goo x y"]@
  , limitResults :: Maybe Int -- ^ Just a limit for results, 'Nothing' for all
  , showPropertySets :: [String] -> String -- ^ function to show property sets.
  , showMoreEI :: Bool
  , showMutantN :: [String] -> a -> a -> String
  , nTestsF :: Int -> Int -- ^ number of tests in function of number of mutants
  , minimumTime :: Int
  }

-- | Default arguments for 'reportWith':
--
-- * @extraMutants = []@, no extra mutants
--
-- * @callNames = []@, use internal default function call template:
--
--   > ["f x y z w x' y' z' ...","g ...","h ...","f' ...",...]
--
-- * @limitResults = Nothing@, show all results
--
-- * @showPropertySets = unwords@, just join property-sets by spaces
--   Other good values for this might be:
--
--   > unlines            -- one per line
--   > unwords . take 5   -- separated by spaces, limit to 5
--   > unlines . take 5   -- one per line, limit to 5
--   > take 30 . unwords  -- limit to 30 characters
--
-- * @nTests = (*2)@, use the same number of tests and mutants
--   There is no general rule of thumb for this function,
--   in some applications, less is better,
--   in some applications, more is better.
--   Increase this if you spot a false positive.
--   Other good values for this might be:
--
--   > (*100)        -- more tests, less false positives
--   > (`div` 100)   -- less tests, less false negatives
--   > (const 1000)  -- specific number of tests
args :: ShowMutable a => Args a
args = Args { extraMutants = []
            , callNames = []
            , limitResults = Nothing    -- show everything
            , showPropertySets = unwords -- just join by spaces
            , showMoreEI = False
            , showMutantN = Mutate.Show.showMutantN
            , nTestsF = (*2)
            , minimumTime = 0
            }

showMutant :: Args a -> a -> a -> String
showMutant as = (showMutantN as) (callNames as)

-- | Report minimality and completeness results.  Uses the standard
--   configuration (see 'args').  Needs the number of mutants to be generated,
--   a function to be mutated and a property map.
report :: (ShowMutable a, Mutable a)
       => Int -> a -> (Int -> a -> [Bool]) -> IO ()
report = reportWith args

-- | Same as 'report' but extra mutants and custom function names can be passed
--   via 'args'.
reportWith :: Mutable a
           => Args a
           -> Int
           -> a
           -> (Int -> a -> [Bool])
           -> IO ()
reportWith args nm f pmap =
  do let nt = nTestsF args nm
     unless (and . pmap nt $ f)
            (putStrLn $ "WARNING: The original function does not follow the property set for "
                     ++ show nt ++ "tests")

     (n,results) <- lastTimeout (minimumTime args) resultss
     let nm = length . survivors $ head results
         nt = nTestsF args nm

     putStrLn $ "Results according to " ++ show nm ++ " mutant variations"
                     ++ " and at most " ++ show nt ++ " test cases:\n"
     putStrLn . table "   "
              . intersperse [ "\n" ]
              . (["Prop. sets", "No.", "Smallest survivor"]:)
              . map showResult
              . maybe id take (limitResults args)
              $ results
     putStrLn $ "Conjectures based on " ++ show nt ++ " test cases"
                     ++ " for each of " ++ show nm ++ " mutant variations:"

     putStrLn . concatMap (showEI)
              . (if showMoreEI args
                   then id
                   else reduceImplications
                      . filterNonCanon
                      . reverse)
              . map (\r -> (sets r, implied r)) -- TODO: Fix this
              $ results
  where
    resultss = map (\n -> let results = getResultsExtra (extraMutants args) n f (pmap (nTestsF args n))
                          in  results `seq` (n,results))
                   (iterate (\x -> x + x `div` 2) nm)
    showResult r = [ showI $ sets r -- ++ "==>" show (implied r)
                   , show  $ nSurvivors r
                   , showM $ smallestSurvivor r
                   ]
    showI = showPropertySets args . map show
    showM (Nothing) = ""
    showM (Just m)  = (showMutant args) f m


showEI :: ([[Int]],[Int]) -> String
showEI ([],_)   = error "shoow: empty property-set equivalence class"
showEI (p:ps,i) = unlines $ map (\p' -> show p ++ " = " ++ show p') ps
                         ++ [ show p ++ " ==> " ++ show i | (not.null) i ]

filterNonCanon :: [([[Int]],[Int])] -> [([[Int]],[Int])]
filterNonCanon [] = []
filterNonCanon ((p:ps,i):rest) = ((p:ps,i):)
                    . filterNonCanon
                    . filter (not . null . fst)
                    . map (mapFst removeNonCanon)
                    $ rest
  where removeNonCanon = filter (not . (\p' -> (p' `contains`) `any` ps))
        mapFst f (x,y) = (f x, y)

reduceImplications :: [([[Int]],[Int])] -> [([[Int]],[Int])]
reduceImplications [] = []
reduceImplications (x:xs) = x : map (x `reduce`) (reduceImplications xs)
  where (ps,i) `reduce` (ps',i') = if or (productWith contained ps ps')
                                     then (ps', i' \\ i)
                                     else (ps', i')
        productWith f xs ys = [f x y | x <- xs, y <- ys]

filterRelevant :: [([[Int]],[Int])] -> [([[Int]],[Int])]
filterRelevant = filterU relevant
  where (p:_,i) `relevant` (ps,i') = not $ i == i'
                                        && any (p `contained`) ps


data Result a = Result
              { sets :: [[Int]]
              , implied :: [Int]
              , survivors :: [Maybe a]
              }
type Results a = [Result a]

smallestSurvivor :: Result a -> Maybe a
smallestSurvivor = listToMaybe . catMaybes . survivors

nSurvivors :: Result a -> Int
nSurvivors = length . catMaybes . survivors


-- | Return minimality and completeness results.  See 'report'.
getResults :: (Mutable a)
           => Int -> a -> (a -> [Bool])
           -> Results a
getResults = getResultsExtra []

getResultsExtra :: (Mutable a)
                => [a]
                -> Int -> a -> (a -> [Bool])
                -> Results a
getResultsExtra ems nms f = map (uncurry process)
                          . getRawResults ems nms f
  where process iss mms = Result { sets      = relevantPropertySets iss
                                 , implied   = relevantImplications iss
                                 , survivors = mms
                                 }

relevantPropertySets :: Eq i => [[i]] -> [[i]]
relevantPropertySets = filterU (not ... contained) . sortOn length

relevantImplications :: Eq i => [[i]] -> [i]
relevantImplications iss = foldr union [] iss
                        \\ foldr union [] (relevantPropertySets iss)

countSurvivors :: [Maybe a] -> Int
countSurvivors = length . catMaybes

minimalMutant :: [Maybe a] -> Maybe a
minimalMutant = listToMaybe . catMaybes

getRawResults :: (Mutable a)
              => [a] -> Int -> a -> (a -> [Bool])
              -> [([[Int]],[Maybe a])]
getRawResults ems nms f pmap = map (mapSnd (zipWith boolToMaybe ms))
                             $ pssurv is pmap ms
  where is = [1..(length (pmap f))]
        ms = take nms (tail $ mutants f)
          ++ ems
        mapSnd f (x,y) = (x,f y)

-- | Returns a description of property sets, grouping the ones that had the
--   same surviving mutants.  The resulting list is ordered starting with the
--   least surviving mutants to the most surviving mutants.
-- Arguments:
--
-- * @is@: list of property ids (@length is == length (pmap x)@)
--
-- * @pmap@: a property map
--
-- * @ms@: list of mutants to apply to the property map
--
-- Return a list of tuples containing:
--
--   * a list of property sets
--   * a boolean list indicating wether a given mutant survived
--
-- > length (pssurv is pmap ms) == length (pmap f)
pssurv :: [i] -> (a -> [Bool]) -> [a] -> [([[i]],[Bool])]
pssurv is pmap = sortOn (count id . snd)
               . map collapse
               . sortAndGroupOn snd
               . zip (subsets is)
               . transpose
               . map (compositions . pmap)
  where collapse [] = error "this should not happen"
        collapse rs@((_,hs):_) = (map fst rs, hs)

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
-- version of 'pssurv' to serve as documentation.
nSurv :: (a -> [Bool]) -> [a] -> [Int]
nSurv props = map (count id)
            . transpose
            . map (compositions . props)
