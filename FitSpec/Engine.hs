-- | FitSpec: refining property-sets for functional testing
--
-- This is the main engine, besides FitSpec.Mutable.
module FitSpec.Engine
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
  )
where

import Test.Check.Error
import Test.Check.Utils
import Data.List ((\\), intercalate, intersperse, union, transpose)
import Data.Ord
import Data.Monoid
import Data.Maybe (catMaybes, listToMaybe, isJust, isNothing)
import Control.Monad (unless)
import FitSpec.Mutable
import FitSpec.ShowMutable (ShowMutable)
import qualified FitSpec.ShowMutable (showMutantN)
import FitSpec.Utils
import FitSpec.PrettyPrint

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
  , showVeryWeakEI :: Bool -- ^ show hidden equivalences and implications
  , showMutantN :: [String] -> a -> a -> String -- ^ special mutant show
  }

nTests :: Args a -> Int
nTests as = nTestsF as (nMutants as)

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
            , showVeryWeakEI = False
            , showMoreEI = False
            , showMutantN = FitSpec.ShowMutable.showMutantN
            }
-- TODO: change showMutantN name to avoid clash with showMutantN

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

propertyHolds :: Int -> Property -> Bool
propertyHolds n = and . map fst . take n

propertyCE :: Int -> Property -> Maybe String
propertyCE n = listToMaybe . map (unwords . snd) . filter (not . fst) . take n

propertiesToMap :: [Property] -> Int -> [Bool]
propertiesToMap ps n = map (propertyHolds n) ps

propertiesHold :: Int -> [Property] -> Bool
propertiesHold n = and . map (propertyHolds n)

propertiesCE :: Int -> [Property] -> Maybe String
propertiesCE n = listToMaybe
               . catMaybes
               . zipWith (\n -> fmap ((show n ++ ":  ") ++)) [1..]
               . map (propertyCE n)

propertiesNTests :: Int -> [Property] -> [Int]
propertiesNTests n = map (length . take n)

propertiesTestsExhausted :: Int -> [Property] -> [Bool]
propertiesTestsExhausted n = map (< n) . propertiesNTests n

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
reportWith args f properties = do
  let nm = nMutants args
      nt = nTestsF args nm
  case propertiesCE nt (properties f) of
    Nothing -> reportWith' args f properties
    Just ce -> do
      putStrLn $ "ERROR: The original function-set does not follow property-set for "
              ++ show nt ++ " tests"
      putStrLn $ "Counter-example to property " ++ ce
      putStrLn $ "Aborting."

-- | Same as 'reportWith', does not abort if the original function does not
--   follow the property set.
reportWith' :: Mutable a
            => Args a
            -> a
            -> (a -> [Property])
            -> IO ()
reportWith' args f properties = do
  results <- lastTimeout (minimumTime args) resultss
  let nm = totalMutants $ head results
      nt = nTestsF args nm
      nts = propertiesNTests nt (properties f)

  putStrLn $ "Results based on"
  putStr   . unlines
           . sortGroupAndCollapse fst snd (\n ps -> "  "
                                                 ++ showNTests n
                                                 ++ " for "
                                                 ++ showProperties ps)
           $ zip nts [1..]
  putStrLn $ "  for each of " ++ show nm ++ " mutant variations.\n"

  putStrLn . table "   "
           . intersperse [ "\n" ]
           . ([ "Property\n sets"
              , "#Survivors\n (%Killed)"
              , "Smallest or simplest\n surviving mutant"
              ]:)
           . map showResult
           . maybe id take (limitResults args)
           $ results

  let eis = showEIs (showVeryWeakEI args) (showMoreEI args) results
  putStrLn $ if null eis
    then "No conjectures."
    else "Conjectures based on " ++ showNTM (sum nts) nm ++ ":"
  putStrLn (table " " eis)
  where
    pmap n f = propertiesToMap (properties f) n
    resultss = takeWhileIncreasingOn (totalMutants . head)
             $ map (\n -> let results = getResultsExtra (extraMutants args) n f (pmap (nTestsF args n))
                          in foldr seq results results) -- evaluate head -> evaluate trunk
                   (iterate (\x -> x + x `div` 2) (nMutants args))
    showResult r = [ showI $ sets r -- ++ "==>" show (implied r)
                   , show  (nSurvivors r) ++ " (" ++ show (score r) ++ "%)"
                   , showM $ smallestSurvivor r
                   ]
    showI = showPropertySets args . map show
    showM (Nothing) = ""
    showM (Just m)  = showMutant args f m
    showNTests 1 =          "1 test case"
    showNTests n = show n ++ " test cases"
    showProperties [p] = "property "   ++ show p
    showProperties ps  = "each of properties "
                      ++ intercalate ", " (map show $ init ps)
                      ++ " and "
                      ++ show (last ps)
    showNTM nt nm = showNTests nt ++ " for each of "
                 ++ show nm ++ " mutant variations"



showEIs :: Bool -> Bool -> [Result a] -> [[String]]
showEIs showVeryWeak showMore =
    concatMap showEI
  . sortOn (abs . (50-) . score)
  . (if showVeryWeak
       then id
       else filter (\r -> nKilled r    /= 0
                       && nSurvivors r /= 0))
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
                 | s < 11 || 89 < s = "(weak)"
                 | s < 33 || 67 < s = "(mild)"
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
               . sortAndGroupFstBySnd
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
