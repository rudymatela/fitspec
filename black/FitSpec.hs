-- | FitSpec tries to find minimal and complete subsets of a specification via
--   black-box mutation testing.
--
-- NOTE:
--   * The API is likely to change in the near future.
--
-- Example -- properties over not:
--
-- > import Test.Check
-- > import MinProps
-- >
-- > propertyMap :: Int -> (Bool -> Bool) -> [Bool]
-- > propertyMap not n =
-- >   [ holds n $ \p -> not (not p) == p
-- >   , holds n $ \p -> not p /= p
-- >   ,                 not True == False
-- >   ]
-- >
-- > main = report not pMap
--
-- FitSpec should report that the minimal (equivalent) specifications are
-- either:
--
--   * 2
--   * 1 and 3

module FitSpec
  ( Args(..)
  , args
  , fixargs
  , report
  , reportWith
  , getRawResults
  , relevantImplications
  , relevantPropertySets
  , ShowMutable
  , property
  , propertyE
  , Property
  , propertiesToMap -- remove this export
  )
where

import Test.Check
import Test.Check.Utils
import Data.List ((\\), intersperse, union, transpose)
import Data.Ord
import Data.Monoid
import Data.Maybe (catMaybes, listToMaybe, isJust, isNothing)
import Control.Monad (unless)
import Mutate
import Mutate.Show (ShowMutable)
import qualified Mutate.Show (showMutantN)
import Utils
import PPPrint

-- | Extra arguments / configuration for 'reportWith'.
--   See 'args' for default values.
data Args a = Args
  { nMutants    :: Int    -- ^ (starting) number of black-box mutations
  , minimumTime :: Int    -- ^ minimum time to run, use 0 for just nMutants
  , nTestsF :: Int -> Int -- ^ number of tests in function of number of mutants
  , callNames :: [String] -- ^ function call templates: @["foo x y","goo x y"]@
  , limitResults :: Maybe Int -- ^ Just a limit for results, 'Nothing' for all

  -- * advanced options:
  , extraMutants :: [a]   -- ^ extra mutants to try to kill alongside mutations
  , showPropertySets :: [String] -> String -- ^ function to show property sets.
  , showMoreEI :: Bool    -- ^ show more equivalences and implications
  , showMutantN :: [String] -> a -> a -> String -- ^ special mutant show
  }

-- | Default arguments for 'reportWith':
--
-- * @nMutants = 500@,
--   start with 500 mutants
--
-- @ @minimumTime = 5@,
--   keep incresing the number of mutants until 5 seconds elapse
--
-- * @nTestsF = (*2)@,
--   use 2 tests more than mutants
--   As a rule of thumb, the number of tests should be proportional to the
--   number of mutants.
--   There is *no* general rule of thumb for the exact proportion:
--   in some applications, less is better,
--   in some applications, more is better.
--   Increase this if you spot a false positive.
--   Other good values for this might be:
--
--   > (*100)        -- more tests, less false positives
--   > (`div` 100)   -- less tests, less false negatives
--   > (const 1000)  -- specific number of tests
--
-- * @callNames = []@,
--   use internal default function call template:
--
--   > ["f x y z w x' y' z' ...","g ...","h ...","f' ...",...]
--
-- * @limitResults = Just 3@,
--   limit to just 3 results
--
-- * @extraMutants = []@,
--   no extra mutants
--
-- * @showPropertySets = unwords@,
--   just join property-sets by spaces.
--   Other good values for this might be:
--
--   > unlines            -- one per line
--   > unwords . take 5   -- separated by spaces, limit to 5
--   > unlines . take 5   -- one per line, limit to 5
--   > take 30 . unwords  -- limit to 30 characters
args :: ShowMutable a => Args a
args = Args { nMutants = 500
            , minimumTime = 5  -- seconds
            , nTestsF = (*2)
            , callNames = []
            , limitResults = Just 3

            , extraMutants = []
            , showPropertySets = unwords -- join by spaces
            , showMoreEI = False
            , showMutantN = Mutate.Show.showMutantN
            }

-- Non timed-out default arguments.
-- Make conjectures based on a fixed number of mutants and tests, e.g.:
--
-- > reportWith (fixargs 100 200) f pmap
--
-- This is just a shorthand, see:
--
-- > fixargs nm nt == args { nMutants = nm, nTestsF = const nt, minimumTime = 0 }
--
-- > (fixargs nm nt) { nMutants = 500, minimumTime = 5, nTestsF = (*2) } == args
fixargs :: ShowMutable a => Int -> Int -> Args a
fixargs nm nt = args
  { nMutants    = nm
  , nTestsF     = const nt
  , minimumTime = 0
  }

showMutant :: Args a -> a -> a -> String
showMutant as = showMutantN as (callNames as)


type Property = [(Bool,[String])]
type Properties = [Property]

property :: Testable a
         => a -> Property
property = resultArguments

propertyE :: Testable a
          => a -> Property
propertyE = map (errorToFalse *** id) . resultArguments

propertyHold :: Int -> Property -> Bool
propertyHold n = and . map fst . take n

propertyCE :: Int -> Property -> Maybe String
propertyCE n = listToMaybe . map (unwords . snd) . filter (not . fst) . take n

propertiesToMap :: [Property] -> Int -> [Bool]
propertiesToMap ps n = map (propertyHold n) ps

propertiesHold :: Int -> [Property] -> Bool
propertiesHold n = and . map (propertyHold n)

propertiesCE :: Int -> [Property] -> Maybe String
propertiesCE n = listToMaybe
               . catMaybes
               . zipWith (\n -> fmap ((show n ++ ".  ") ++)) [1..]
               . map (propertyCE n)


-- | Report minimality and completeness results.
--   Uses standard configuration (see 'args').
--   Needs a function to be mutated and a property map.
report :: (ShowMutable a, Mutable a)
       => a -> (a -> [Property]) -> IO ()
report = reportWith args

-- | Same as 'report' but can be configured via 'Args'/'args'.
reportWith :: Mutable a
           => Args a
           -> a
           -> (a -> [Property])
           -> IO ()
reportWith args f properties =
  do let nm = nMutants args
         nt = nTestsF args nm
     case propertiesCE nt (properties f) of
       Nothing -> return () -- Just carry on
       Just ce -> do
         putStrLn $ "WARNING: The original function does not follow the property set for "
                 ++ show nt ++ " tests"
         putStrLn $ "         property " ++ ce

     (n,results) <- lastTimeout (minimumTime args) resultss
     let nm = totalMutants $ head results
         nt = nTestsF args nm

     putStrLn $ "Results based on " ++ showNTM nt nm ++ ".\n"
     putStrLn . table "   "
              . intersperse [ "\n" ]
              . ([ "Property\n sets"
                 , "#Survivors\n (%Killed)"
                 , "Smallest or simplest\n surviving mutant"
                 ]:)
              . map showResult
              . maybe id take (limitResults args)
              $ results

     let eis = showEIs (showMoreEI args) results
     putStrLn $ if null eis
       then "No conjectures."
       else "Conjectures based on " ++ showNTM nt nm ++ ":"
     putStrLn (table " " eis)
  where
    pmap n f = propertiesToMap (properties f) n
    resultss = takeWhileIncreasingOn (totalMutants . head . snd)
             $ map (\n -> let results = getResultsExtra (extraMutants args) n f (pmap (nTestsF args n))
                          in foldr seq (n,results) results)
                   (iterate (\x -> x + x `div` 2) (nMutants args))
    showResult r = [ showI $ sets r -- ++ "==>" show (implied r)
                   , show  (nSurvivors r) ++ " (" ++ show (score r) ++ "%)"
                   , showM $ smallestSurvivor r
                   ]
    showI = showPropertySets args . map show
    showM (Nothing) = ""
    showM (Just m)  = showMutant args f m
    showNTM nt nm = "at most " ++ show nt ++ " test cases"
            ++ " for each of " ++ show nm ++ " mutant variations"


showEIs :: Bool -> [Result a] -> [[String]]
showEIs showMore = concatMap showEI
                 . (if showMore
                      then id
                      else reduceImplications
                         . filterNonCanon
                         . reverse)


showEI :: Result a -> [[String]]
showEI r = map (\p' -> [show p, " = ", show p', "   ", show s ++ "% killed", sMeaning]) ps
        ++ [ [show p, "==>", show i, "   ", show s ++ "% killed", sMeaning] | (not.null) i ]
  where (p:ps) = sets r
        i      = implied r
        s      = score r
        sMeaning | s < 1  || 99 < s = "(very weak)"
                 | s < 4  || 96 < s = "(quite weak)"
                 | s < 10 || 90 < s = "(weak)"
                 | s < 25 || 75 < s = "(mild)"
                 | otherwise        = "(strong)" -- the closer to 50 the better
-- TODO: improve implication score
-- implication score can be improved by
-- by separating each implication on its own:
--   [4] ==> [2,3]
-- become
--   [4] ==> [2]
--   [4] ==> [3]
-- Then evaluating percentage of occurences of True ==> True and other cases

filterNonCanon :: [Result a] -> [Result a]
filterNonCanon [] = []
filterNonCanon (r:rs) = (r:)
                      . filterNonCanon
                      . filter (not . null . sets)
                      . map (updateSets removeNonCanon)
                      $ rs
  where removeNonCanon = filter (not . (\p' -> (p' `contains`) `any` tail (sets r)))
        updateSets f r = r { sets = f (sets r) }

reduceImplications :: [Result a] -> [Result a]
reduceImplications [] = []
reduceImplications (r:rs) = r : map (r `reduce`) (reduceImplications rs)
  where r `reduce` r' = if or (productWith contained (sets r) (sets r'))
                          then r' { implied = implied r' \\ implied r }
                          else r'
        productWith f xs ys = [f x y | x <- xs, y <- ys]


data Result a = Result
              { sets             :: [[Int]]
              , implied          :: [Int]
              , survivors        :: [Maybe a]
              , smallestSurvivor :: Maybe a
              , nSurvivors       :: Int
              , nKilled          :: Int
              , totalMutants     :: Int
              , score            :: Int
              }
type Results a = [Result a]


-- | Return minimality and completeness results.  See 'report'.
getResults :: (Mutable a)
           => Int -> a -> (a -> [Bool])
           -> Results a
getResults = getResultsExtra []

getResultsExtra :: (Mutable a)
                => [a]
                -> Int -> a -> (a -> [Bool])
                -> Results a
getResultsExtra ems nms f = map (uncurry processRawResult)
                          . getRawResults ems nms f

processRawResult :: [[Int]] -> [Maybe a] -> Result a
processRawResult iss mms = Result
  { sets      = relevantPropertySets iss
  , implied   = relevantImplications iss
  , survivors = mms
  , smallestSurvivor = listToMaybe ms
  , nSurvivors   = ns
  , nKilled      = nk
  , totalMutants = nm
  , score        = nk*100 `div` nm
  }
  where ms = catMaybes mms
        nm = length mms
        ns = length ms
        nk = nm - ns

relevantPropertySets :: Eq i => [[i]] -> [[i]]
relevantPropertySets = filterU (not ... contained) . sortOn length

relevantImplications :: Eq i => [[i]] -> [i]
relevantImplications iss = foldr union [] iss
                        \\ foldr union [] (relevantPropertySets iss)

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
