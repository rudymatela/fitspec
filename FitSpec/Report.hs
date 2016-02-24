module FitSpec.Report
  ( report
  , reportWith
  , Args(..)
  , args
  , fixargs
  , Property
  , ShowMutantAs(..)
  )
where

import Data.List (intercalate, intersperse)

import FitSpec.Engine
import FitSpec.Mutable
import FitSpec.ShowMutable hiding (showMutant)
import FitSpec.Utils
import FitSpec.PrettyPrint

data ShowMutantAs = Tuple      | NestedTuple
                  | Definition | Bindings

-- | Extra arguments / configuration for 'reportWith'.
--   See 'args' for default values.
data Args = Args
  { nMutants :: Int -- ^ (starting) number of function mutations
  , nTests   :: Int -- ^ (starting) number of test values (for each prop.)
  , timeout  :: Int -- ^ timeout in seconds, 0 for just 'nTests' * 'nMutants'
  , names    :: [String] -- ^ names of functions: @["foo x y","goo x y"]@

  -- advanced options:
  , verbose      :: Bool         -- ^ whether to show detailed results
  , showMutantAs :: ShowMutantAs -- ^ how to show mutants
  , rows         :: Maybe Int    -- ^ number of surviving mutants to show
  , extra        :: String       -- ^ ignored argument (user defined meaning)
  }

-- | Number of tests as a function of the number of mutants
nTestsF :: Args -> Int -> Int
nTestsF as nm = nm * nTests as `div` nMutants as

-- | Default arguments for 'reportWith':
--
-- * @nMutants = 500@, start with  500 mutants
--
-- * @nTests = 1000@,  start with 1000 test values
--
-- * @timeout = 5@, keep incresing the number of mutants
--                  until 5 seconds elapse
--
-- * @names = []@, default function call template:
--
--   > ["f x y z w x' y' z' ...","g ...","h ...","f' ...",...]
args :: Args
args = Args { nMutants     =  500
            , nTests       = 1000
            , timeout      = 5  -- seconds
            , names        = []
            , verbose      = False
            , showMutantAs = Tuple
            , rows         = Nothing
            , extra        = ""
            }

-- Non timed-out default arguments.
-- Make conjectures based on a fixed number of mutants and tests, e.g.:
--
-- > reportWith (fixargs 100 200) f pmap
--
-- This is just a shorthand, see:
--
-- > fixargs nm nt == args { nMutants = nm, nTests = nt, timeout = 0 }
--
-- > (fixargs nm nt) { nMutants = 500, nTests = 1000, timeout = 5 } == args
fixargs :: Int -> Int -> Args
fixargs nm nt = args
  { nMutants = nm
  , nTests   = nt
  , timeout  = 0
  }

showMutant :: ShowMutable a => Args -> a -> a -> String
showMutant as = showMutantByType (showMutantAs as) (names as)
  where
    showMutantByType Tuple       = showMutantAsTuple
    showMutantByType NestedTuple = showMutantNested
    showMutantByType Definition  = showMutantDefinition
    showMutantByType Bindings    = showMutantBindings

-- | Report minimality and completeness results.
--   Uses standard configuration (see 'args').
--   Needs a function to be mutated and a property map.
report :: (Mutable a, ShowMutable a)
       => a -> (a -> [Property]) -> IO ()
report = reportWith args


-- | Same as 'report' but can be configured via 'Args'/'args'.
reportWith :: (Mutable a, ShowMutable a)
           => Args -> a -> (a -> [Property]) -> IO ()
reportWith = reportWithExtra []


-- | Same as 'reportWith', but accepts a list of extra mutants to try
reportWithExtra :: (Mutable a, ShowMutable a)
                => [a] -> Args -> a -> (a -> [Property]) -> IO ()
reportWithExtra extraMutants args f properties = do
  let nm = nMutants args
      nt = nTestsF args nm
  case propertiesCE nt (properties f) of
    Nothing -> reportWithExtra' extraMutants args f properties
    Just ce -> do
      putStrLn $ "ERROR: The original function-set does not follow property-set for "
              ++ show nt ++ " tests"
      putStrLn $ "Counter-example to property " ++ ce
      putStrLn $ "Aborting."

-- | Same as 'reportWithExtra', does not abort if the original function does not
--   follow the property set.
reportWithExtra' :: (Mutable a, ShowMutable a)
                 => [a] -> Args -> a -> (a -> [Property]) -> IO ()
reportWithExtra' extraMutants args f properties = do
  let pmap n f = propertiesToMap (properties f) n
      resultss = takeWhileIncreasingOn (totalMutants . head)
               . map (\n -> let rs = getResultsExtra extraMutants n f
                                                     (pmap (nTestsF args n))
                            in foldr seq rs rs) -- eval head -> eval trunk
               $ (iterate (\x -> x + x `div` 2) (nMutants args))
  results <- lastTimeout (timeout args) resultss

  let nm = totalMutants $ head results
      nt = nTestsF args nm
      nts = propertiesNTests nt (properties f)
  putStrLn $ "Apparent " ++ qualifyCM results ++ " specification based on"
  putStrLn $ showNumberOfTestsAndMutants nts nm False

  let showR | verbose args = showDetailedResults (rows args)
            | otherwise    = showResults
  putStrLn $ showR (showMutant args f) results


showResults :: (a -> String)
            -> [Result a] -> String
showResults showMutant rs@(r:_) = completeness
                       ++ "\n" ++ minimality
  where
    completeness = show (nSurvivors r) ++ " survivors ("
                ++ show (score r) ++ "% killed)"
                ++ case smallestSurvivor r of
                     Nothing -> ".\n"
                     Just m  -> ", smallest:\n" ++ "  " `beside` showMutant m
    minimality = "apparent minimal property-sub-sets:  "
              ++ (unwords . map showPropertySet $ sets r) ++ "\n"
              ++ case showConjectures False rs of
                   "" -> "No conjectures.\n"
                   cs -> "conjectures:  " `beside` cs


showDetailedResults :: Maybe Int -> (a -> String)
                    -> [Result a] -> String
showDetailedResults mlimit showMutant rs = completeness
                                ++ "\n" ++ minimality
  where
    completeness = table "   " . intersperse ["\n"]
                 . ([ "Property\n sets"
                    , "#Survivors\n (%Killed)"
                    , "Smallest or simplest\n surviving mutant"
                    ]:)
                 . map showResult
                 . maybe id take mlimit
                 $ rs
    showResult r = [ unwords . map showPropertySet $ sets r
                   , show  (nSurvivors r) ++ " (" ++ show (score r) ++ "%)"
                   , maybe "" showMutant $ smallestSurvivor r
                   ]
    minimality = case showConjectures True rs of
                   "" -> "No conjectures.\n"
                   cs -> "Conjectures:\n" ++ cs


showNumberOfTestsAndMutants :: [Int] -> Int -> Bool -> String
showNumberOfTestsAndMutants nts nm ssum = numTests ++ numMutants
  where
    numMutants = "for each of " ++ showQuantity nm "mutant variation" ++ ".\n"
    numTests | ssum = showQuantity (sum nts) "test case" ++ "\n"
             | otherwise = unlines
                         . sortGroupAndCollapse fst snd testsForProps
                         $ zip nts [1..]
    testsForProps n ps = showQuantity n "test case"
                      ++ " for " ++ showEach "property" ps

showPropertySet :: Show i => [i] -> String
showPropertySet = (\s -> "{" ++ s ++ "}") . intercalate "," . map show


-- | Show conjectures derived from results
showConjectures :: Bool -> [Result a] -> String
showConjectures showVeryWeak =
    table " "
  . concatMap showConjectures1
  . sortOn (abs . (50-) . score)
  . (if showVeryWeak
       then id
       else filter (\r -> nKilled r    /= 0
                       && nSurvivors r /= 0))
  . reduceImplications
  . filterNonCanon
  . reverse


-- | Show conjectures derived from a single result.
showConjectures1 :: Result a -> [[String]]
showConjectures1 r =
           map (++ ["   ", show s ++ "% killed", sMeaning])
         $ [ [showPropertySet p, " = ", showPropertySet p'] | p' <- ps ]
        ++ [ [showPropertySet p, "==>", showPropertySet i ] | (not.null) i ]
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

