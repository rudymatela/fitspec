-- | FitSpec: refining property-sets for functional testing
--
-- This is the main engine, besides FitSpec.Mutable.
module FitSpec.Engine
  ( property
  , propertyE
  , Property

  , getResults
  , getResultsExtra
  , Result (..)
  , Results

  , propertiesNTests
  , propertiesToMap
  , propertiesHold
  , propertiesCE

  , minimal
  , complete

  , reduceImplications
  , filterNonCanon

  -- TODO: Remove export of relevantPS, relevantI and getRaw (fix FitSpec.Dot)
  , relevantPropertySets
  , relevantImplications
  , getRawResults
  )
where

import Test.Check.Error
import FitSpec.Utils
import Data.Maybe (catMaybes, listToMaybe)
import Data.List ((\\),union,transpose)
import FitSpec.Mutable

type Property = [([String],Bool)]
type Properties = [Property]

property :: Testable a
         => a -> Property
property = results

propertyE :: Testable a
          => a -> Property
propertyE = map (id *** errorToFalse) . property

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
propertiesTestsExhausted n = map (< n) . propertiesNTests n

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
