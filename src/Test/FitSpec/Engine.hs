-- |
-- Module      : Test.FitSpec.Engine
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- FitSpec: refining property-sets for functional testing
--
-- This is the main engine, besides "Test.FitSpec.Mutable".
module Test.FitSpec.Engine
  ( property
  , Property

  , getResults
  , getResultsExtra
  , getResultsExtraTimeout
  , Result (..)
  , Results

  , propertiesNTests
  , propertiesTestsExhausted
  , propertiesToMap
  , propertiesHold
  , propertiesCE

  , minimal
  , complete

  , reduceImplications
  , filterNonCanon
  , Conjecture (..)
  , conjectures
  )
where

import Test.LeanCheck.Error
import Test.FitSpec.Utils
import Data.Maybe (catMaybes, listToMaybe)
import Data.List ((\\),union,transpose)
import Test.FitSpec.Mutable

-- | An encoded representation of a property suitable for use by FitSpec.
--
-- Each list of strings is a printable representation of one possible choice of
-- argument values for the property.  Each boolean indicate whether the
-- property holds for this choice.
type Property = [([String],Bool)]
type Properties = [Property]

-- | Given a 'Testable' type (as defined by "Test.LeanCheck"), returns a 'Property'.
--
-- This function should be used on every property to create a property list to
-- be passed to 'report', 'reportWith', 'mainDefault' or 'mainWith'.
--
-- > property $ \x y -> x + y < y + (x::Int)
property :: Testable a => a -> Property
property = results

propertyHolds :: Int -> Property -> Bool
propertyHolds n = all snd . take n

propertyCE :: Int -> Property -> Maybe String
propertyCE n = listToMaybe . map (unwords . fst) . filter (not . snd) . take n

propertiesToMap :: [Property] -> Int -> [Bool]
propertiesToMap ps n = map (propertyHolds n) ps

propertiesHold :: Int -> [Property] -> Bool
propertiesHold n = all (propertyHolds n)

propertiesCE :: Int -> [Property] -> Maybe String
propertiesCE n = listToMaybe
               . catMaybes
               . zipWith (\n -> fmap ((show n ++ ":  ") ++)) [1..]
               . map (propertyCE n)

propertiesNTests :: Int -> [Property] -> [Int]
propertiesNTests n = map (length . take n)

propertiesTestsExhausted :: Int -> [Property] -> [Bool]
propertiesTestsExhausted n = map (<= n) . propertiesNTests (n+1)

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
  where r `reduce` r' = if or [s `contained` s' | s <- sets r, s' <- sets r']
                          then r' { implied = implied r' \\ implied r }
                          else r'


-- | A line of result for a single equivalence class of properties
--   with the exact same surviving mutants.
data Result a = Result
  { sets             :: [[Int]] -- ^ property-sets in the equivalence class
  , implied          :: [Int]   -- ^ properties implied by this class
  , survivors        :: [a]     -- ^ list of surviving mutants
  , smallestSurvivor :: Maybe a -- ^ smallest surviving mutant, if any
  , nSurvivors       :: Int -- ^ number of surviving mutants
  , nKilled          :: Int -- ^ number of killed mutants
  , totalMutants     :: Int -- ^ total number of mutants generated and tested
  , score            :: Int -- ^ percentage of killed mutants, 0-100
  , maxTests         :: Int -- ^ Requested number of tests (same for all rs.)
  , mutantsExhausted :: Bool -- ^ mutants were exhausted
  }
type Results a = [Result a]


-- | Return minimality and completeness results.  See 'report'.
getResults :: (Mutable a)
           => a -> (a -> [Property]) -> Int -> Int
           -> Results a
getResults = getResultsExtra []

getResultsExtra :: (Mutable a)
                => [a]
                -> a -> (a -> [Property]) -> Int -> Int
                -> Results a
getResultsExtra ems f ps nms nts = map (uncurry $ processRawResult mex nts)
                                 $ getRawResults is pmap ms
  where is = [1..(length $ ps f)]
        pmap f = propertiesToMap (ps f) nts
        ms' = take (nms+1) (tail $ mutants f)
        mex = length ms' <= nms
        ms = take nms ms' ++ ems

getResultsExtraTimeout :: (Mutable a)
                       => Int
                       -> [a]
                       -> a -> (a -> [Property]) -> Int -> Int
                       -> IO (Results a)
getResultsExtraTimeout 0 ems f ps m n = return $ getResultsExtra ems f ps m n
getResultsExtraTimeout t ems f ps nm0 nt0 = lastTimeout t resultss
  where
    resultss = map fst
             $ takeWhileIncreasingOn ((totalMutants . head) *** id)
             [ (getResultsExtra ems f ps nm nt, propertiesNTests nt $ ps f)
             | (nm,nt) <- iterate (incHalf *** incHalf) (nm0,nt0) ]
    incHalf x = x + x `div` 2

processRawResult :: Bool -> Int -> [[Int]] -> [(a,Bool)] -> Result a
processRawResult mex nt iss mhs = Result
  { sets      = relevantPropertySets iss
  , implied   = relevantImplications iss
  , survivors = ms
  , smallestSurvivor = listToMaybe ms
  , nSurvivors   = ns
  , nKilled      = nk
  , totalMutants = nm
  , score        = nk*100 `div` nm
  , maxTests     = nt
  , mutantsExhausted = mex
  }
  where ms = [m | (m,h) <- mhs, h]
        nm = length mhs
        ns = length ms
        nk = nm - ns

minimal :: Results a -> Bool
minimal (r:_) = null (implied r)
             && length (sets r) == 1

complete :: Results a -> Bool
complete (r:_) = nSurvivors r == 0

relevantPropertySets :: Eq i => [[i]] -> [[i]]
relevantPropertySets = filterU (not ... contained) . sortOn length

relevantImplications :: Eq i => [[i]] -> [i]
relevantImplications iss = foldr union [] iss
                        \\ foldr union [] (relevantPropertySets iss)

-- | Returns a description of property sets, grouping the ones that had the
--   same surviving mutants.  The resulting list is ordered starting with the
--   least surviving mutants to the most surviving mutants.
--
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
--   * a list of mutants paired with booleans indicating whether each survived
getRawResults :: [i] -> (a -> [Bool]) -> [a] -> [([[i]],[(a,Bool)])]
getRawResults is ps ms = (id *** (zip ms)) `map` getRawResults' is ps ms

-- | Returns a description of property sets, grouping the ones that had the
--   same surviving mutants.  The resulting list is ordered starting with the
--   least surviving mutants to the most surviving mutants.
--
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
--   * a boolean list indicating whether a given mutant survived
getRawResults' :: [i] -> (a -> [Bool]) -> [a] -> [([[i]],[Bool])]
getRawResults' is pmap = sortOn (count id . snd)
                       . sortAndGroupFstBySnd
                       . zip (subsets is)
                       . transpose
                       . map (compositions . pmap)

-- | @nSurv props fs@ returns the number of values that match
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
--
-- It is also not exported!
nSurv :: (a -> [Bool]) -> [a] -> [Int]
nSurv props = map (count id)
            . transpose
            . map (compositions . props)


data Conjecture = Conjecture
  { isEq        :: Bool
  , isIm        :: Bool
  , cleft       :: [Int]
  , cright      :: [Int]
  , cscore      :: Int
  , cnKilled    :: Int
  , cnSurvivors :: Int
  } deriving Show

conjectures :: [Result a] -> [Conjecture]
conjectures = concatMap conjectures1
            . sortOn (abs . (50-) . score) -- closer to 50 the better!
            . reduceImplications
            . filterNonCanon
            . reverse

conjectures1 :: Result a -> [Conjecture]
conjectures1 r = [ p `eq` p' | p' <- ps ]
              ++ [ p `im` i  | (not.null) i ]
  where
    (p:ps) = sets r
    i      = implied r
    eq     = conj True
    im     = conj False
    conj isE p p' = Conjecture
      { isEq        = isE
      , isIm        = not isE
      , cleft       = p
      , cright      = p'
      , cscore      = score r
      , cnKilled    = nKilled r
      , cnSurvivors = nSurvivors r
      }
-- TODO: improve implication score
-- implication score can be improved by
-- by separating each implication on its own:
--   [4] ==> [2,3]
-- become
--   [4] ==> [2]
--   [4] ==> [3]
-- Then evaluating percentage of occurences of True ==> True and other cases
