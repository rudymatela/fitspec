module FitSpec.Report
  ( report
  , reportWith
  , Args(..)
  , args
  , fixargs
  , Property
  )
where

{-
import Data.List ((\\), intercalate, intersperse, union, transpose)
import Data.Ord
import Control.Monad (unless)
import FitSpec.ShowMutable (ShowMutable, showMutantAsTuple)
import FitSpec.PrettyPrint
-}
import Data.List (intercalate, intersperse)

import FitSpec.Engine
import FitSpec.Mutable
import FitSpec.ShowMutable hiding (showMutant)
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

  -- advanced options:
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
            , showMutantN = showMutantAsTuple
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

  putStrLn $ "Apparent " ++ qualifyCM results ++ " specification based on"
  putStr   . unlines
           . sortGroupAndCollapse fst snd
               (\n ps -> showNTests n ++ " for " ++ showProperties ps)
           $ zip nts [1..]
  putStrLn $ "for each of " ++ show nm ++ " mutant variations.\n"

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
    showM Nothing  = ""
    showM (Just m) = showMutant args f m
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

qualifyCM :: Results a -> String
qualifyCM rs | c && m    = "complete and minimal"
             | c         = "complete but non-minimal"
             |      m    = "minimal but incomplete"
             | otherwise = "incomplete and non-minimal"
  where c = complete rs
        m = minimal rs

