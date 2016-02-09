-- | Alternate implementation fo FitSpec.
-- this classifies mutants into two categories:
-- ones that has un-exercised mutated cases;
-- ones that did have not.
--
-- This version is much less flexible and is still very experimental.
--
-- It is called "Grey" because, despite being mostly black-box,
-- it catches evalutaion of mutated parts by raising errors
-- inside functions (unsafePerformIO).
-- Not totally black-box, neither totally white-box.
module FitSpec.Grey where

import Test.Check
import Test.Check.Utils
import Data.List
import Data.Ord
import Data.Monoid
import Data.Maybe (catMaybes, listToMaybe)
import FitSpec.PrettyPrint
import FitSpec.Grey.Mutation
import FitSpec.Utils
import Data.Either (lefts, rights)
import Control.Applicative (liftA2)
import FitSpec.Grey.CatchEvaluation

-- | Extra arguments for 'getResultsWith' and 'reportWith'
data CArgs = CArgs
           { functionNames :: [String]
           , variableNames :: [String]
           , nResults :: Maybe Int
           }

-- | Default arguments
cargs = CArgs { functionNames = ["f","g","h"] ++ repeat "function"
              , variableNames = ["x","y","z"] ++ repeat "x"
              , nResults = Nothing -- show everything
              }

getResults1 :: (Eq a, Eq b, Listable a, Listable b)
            => Int -> (a -> b) -> ((a -> b) -> [Bool])
            -> [([Int], Int, Maybe [(a,b)], Int, Maybe [(a,b)])]
getResults1 nMuts f propMap = nSurvCT pids (propEvalMap propMap f) mns
  where pids = [1..(length (propMap f))]
        mns = take nMuts (tail $ mutations f)

getResults2 :: (Eq a, Eq b, Eq c, Eq d, Listable a, Listable b, Listable c, Listable d)
            => Int -> (a -> b) -> (c -> d)
            -> ((a -> b) -> (c -> d) -> [Bool])
            -> [([Int], Int, Maybe (Mutation a b, Mutation c d), Int, Maybe (Mutation a b, Mutation c d))]
getResults2 nMuts f g propMap = nSurvCT pids (uncurry (propEvalMap2 propMap f g)) mns
  where pids = [1..(length (propMap f g))]
        mns = take nMuts (tail $ mutations2 f g)

getResults3 :: ( Eq a, Eq b, Eq c, Eq d, Eq e, Eq f
               , Listable a, Listable b, Listable c, Listable d, Listable e, Listable f )
            => Int -> (a -> b) -> (c -> d) -> (e -> f)
            -> ((a -> b) -> (c -> d) -> (e -> f) -> [Bool])
            -> [( [Int]
                , Int, Maybe (Mutation a b, Mutation c d, Mutation e f)
                , Int, Maybe (Mutation a b, Mutation c d, Mutation e f) )]
getResults3 nMuts f g h propMap = nSurvCT pids (uncurry3 (propEvalMap3 propMap f g h)) mns
  where pids = [1..(length (propMap f g h))]
        mns = take nMuts (tail $ mutations3 f g h)

-- Takes a property map and retuns a map that tells wether a given mutant:
--
--   * Fails: Nothing
--   * Survives: Just True
--   * Survives, but is a duplicate: Just False
propEvalMap :: Eq a
            => ((a -> b) -> [Bool])
            -> (a -> b)
            -> [(a,b)]
            -> [Maybe Bool]
propEvalMap propMap f bs = zipWith boolToMaybe
                                   (evalMap propMap f bs)
                                   (propMap (makeMutant f bs))

propEvalMap2 :: (Eq a, Eq c)
             => ((a -> b) -> (c -> d) -> [Bool])
             -> (a -> b) -> (c -> d)
             -> Mutation a b -> Mutation c d
             -> [Maybe Bool]
propEvalMap2 propMap f g bs cs = zipWith boolToMaybe
                                         (evalMap2 propMap f g bs cs)
                                         (propMap (makeMutant f bs) (makeMutant g cs))

propEvalMap3 :: (Eq a, Eq c, Eq e)
             => ((a -> b) -> (c -> d) -> (e -> f) -> [Bool])
             -> (a -> b) -> (c -> d) -> (e -> f)
             -> Mutation a b -> Mutation c d -> Mutation e f
             -> [Maybe Bool]
propEvalMap3 propMap f g h bs cs ds =
  zipWith boolToMaybe
          (evalMap3 propMap f g h bs cs ds)
          (propMap (makeMutant f bs) (makeMutant g cs) (makeMutant h ds))

-- Takes a property map and returns a map that tells wether a given mutant is
-- evaluated by each of the properties
evalMap :: Eq a
        => ((a -> b) -> [Bool])
        -> (a -> b)
        -> [(a,b)]
        -> [Bool]
evalMap propMap f bs = foldr (zipWith (&&)) (repeat True)
                     $ map (map catchedE . propMap)
                           (makeMutantTraps f bs)

evalMap2 :: (Eq a, Eq c)
         => ((a->b) -> (c->d) -> [Bool])
         -> (a->b) -> (c->d)
         -> Mutation a b -> Mutation c d
         -> [Bool]
evalMap2 propMap f g fbs gbs = foldr (zipWith (&&)) (repeat True)
                             $ map (map catchedE . uncurry propMap)
                                   (makeMutantTraps2 f g fbs gbs)

evalMap3 :: (Eq a, Eq c, Eq e)
         => ((a->b) -> (c->d) -> (e->f) -> [Bool])
         -> (a->b) -> (c->d) -> (e->f)
         -> Mutation a b -> Mutation c d -> Mutation e f
         -> [Bool]
evalMap3 propMap f g h fbs gbs hbs = foldr (zipWith (&&)) (repeat True)
                                   $ map (map catchedE . uncurry3 propMap)
                                         (makeMutantTraps3 f g h fbs gbs hbs)


-- stub report functions for the filtered version:

report1With :: ( Eq a, Eq b
               , Show a, Show b
               , Listable a, Listable b )
            => CArgs
            -> Int
            -> (a -> b)
            -> ((a -> b) -> [Bool])
            -> IO ()
report1With args nMuts f propMap = putStrLn . table "   " . map showResult
                                 $ maybe id take (nResults args)
                                 $ getResults1 nMuts f propMap
  where showResult (x,y,mm,z,mn) = [ show x, show y, showM mm, show z, showM mn ]
        showM Nothing  = ""
        showM (Just m) = showMutant_ (functionNames args !! 0) (variableNames args !! 0) m

report1 :: ( Eq a, Eq b
           , Show a, Show b
           , Listable a, Listable b )
        => Int
        -> (a -> b)
        -> ((a -> b) -> [Bool])
        -> IO ()
report1 = report1With cargs

report2With :: ( Eq a, Eq b, Eq c, Eq d
               , Show a, Show b, Show c, Show d
               , Listable a, Listable b, Listable c, Listable d)
            => CArgs
            -> Int
            -> (a -> b) -> (c -> d)
            -> ((a -> b) -> (c -> d) -> [Bool])
            -> IO ()
report2With args nMuts f g propMap = putStrLn . table "   " . map showResult
                                   $ maybe id take (nResults args)
                                   $ getResults2 nMuts f g propMap
  where showResult (x,y,mm,z,mn) = [ show x, show y, showM mm, show z, showM mn ]
        showM Nothing  = ""
        showM (Just (m,n)) = showMutant_ (functionNames args !! 0) (variableNames args !! 0) m
                          ++ showMutant_ (functionNames args !! 1) (variableNames args !! 1) n

report2 :: ( Eq a, Eq b, Eq c, Eq d
           , Show a, Show b, Show c, Show d
           , Listable a, Listable b, Listable c, Listable d)
        => Int
        -> (a -> b) -> (c -> d)
        -> ((a -> b) -> (c -> d) -> [Bool])
        -> IO ()
report2 = report2With cargs

report3With :: ( Eq a, Eq b, Eq c, Eq d, Eq e, Eq f
               , Show a, Show b, Show c, Show d, Show e, Show f
               , Listable a, Listable b, Listable c, Listable d, Listable e, Listable f)
            => CArgs
            -> Int
            -> (a -> b) -> (c -> d) -> (e -> f)
            -> ((a -> b) -> (c -> d) -> (e -> f) -> [Bool])
            -> IO ()
report3With args nMuts f g h propMap = putStrLn . table "   " . map showResult
                                     $ maybe id take (nResults args)
                                     $ getResults3 nMuts f g h propMap
  where showResult (x,y,mm,z,mn) = [ show x, show y, showM mm, show z, showM mn ]
        showM Nothing  = ""
        showM (Just (m,n,o)) = showMutant_ (functionNames args !! 0) (variableNames args !! 0) m
                            ++ showMutant_ (functionNames args !! 1) (variableNames args !! 1) n
                            ++ showMutant_ (functionNames args !! 2) (variableNames args !! 2) o

report3 :: ( Eq a, Eq b, Eq c, Eq d, Eq e, Eq f
           , Show a, Show b, Show c, Show d, Show e, Show f
           , Listable a, Listable b, Listable c, Listable d, Listable e, Listable f)
        => Int
        -> (a -> b) -> (c -> d) -> (e -> f)
        -> ((a -> b) -> (c -> d) -> (e -> f) -> [Bool])
        -> IO ()
report3 = report3With cargs


nSurvCT :: Eq pid => [pid] -> (a -> [Maybe Bool]) -> [a] -> [([pid],Int,Maybe a,Int,Maybe a)]
nSurvCT pids pmap = filterU relevant
                  . sortBy (comparing $ \(ids,s,_,s',_) -> (s, s', length ids))
                  . zipWith (\ids sms -> ( ids
                                         , length sms,          listToMaybe (eithers sms)
                                         , length (rights sms), listToMaybe (rights sms)))
                            (subsets pids)
                  . map catMaybes
                  . transpose
                  . map (\func -> map (resultToMaybe func)
                                      (compositions' (pmap func)))
   where (is,n,_,m,_) `relevant` (is',n',_,m',_) = not (n == n' && m == m' && is `contained` is')
         resultToMaybe _ Nothing      = Nothing         -- dead mutant
         resultToMaybe f (Just True)  = Just (Right f)  -- survivor
         resultToMaybe f (Just False) = Just (Left  f)  -- survivor, but possible duplicate

compositions' :: [Maybe Bool] -> [Maybe Bool]
compositions' = map (foldr (liftA2 (&&)) (Just True)) . subsets

