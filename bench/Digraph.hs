-- A small library of functions on directed graphs
-- using a simple list-of-successors representation.
-- Colin Runciman, May 2015

module Digraph (Digraph(..), okDigraph, strictOrder,
                sources, targets, nodes, preds, succs,
                isNode, isEdge, isPath,
                emptyDigraph, addNode, addEdge, assoc1toNdigraph,
                transitiveClosure, topoSort,
                insert, union, diff, cycles, subgraph, maxDagFrom) where

import GHC.Exts (groupWith)
import Data.List (partition,(\\),sort)
import Data.Maybe (isJust,fromJust)
import Control.Monad (guard)

data Digraph a = D {nodeSuccs :: [(a,[a])]} deriving (Eq, Show)
-- Data invariant: in a digraph pair-list [...(source,targets)...]:
-- (1) pairs are listed in strictly increasing source order
-- (2) each list of targets is in strictly increasing order
-- (3) every element in a list of targets must itself be
--     listed as a "source", though perhaps with [] targets

okDigraph :: (Ord a, Eq a) => Digraph a -> Bool
okDigraph (D d)  =
  strictOrder ss && all goodTargetList tss
  where
  ss                     =  map fst d
  tss                    =  map snd d
  goodTargetList ts      =  strictOrder ts &&
                            all (`elemOrd` ss) ts

strictOrder :: (Ord a, Eq a) => [a] -> Bool
strictOrder (x:y:etc)  =  x < y && strictOrder (y:etc)
strictOrder _          =  True

nodes :: Digraph a -> [a]
nodes (D d)  =  [s | (s,_) <- d]

sources :: Digraph a -> [a]
sources (D d)  =  [s | (s,ts) <- d, not (null ts)]

targets :: (Ord a, Eq a) => Digraph a -> [a]
targets (D d) =  foldr union [] (map snd d)

preds :: (Ord a, Eq a) => a -> Digraph a -> [a]
preds t (D d)  =  [s | (s,ts) <- d, t `elemOrd` ts]

succs :: (Ord a, Eq a) => a -> Digraph a -> [a]
succs s (D d)  =  case lookup s d of
                  Just ns -> ns
                  Nothing -> []

isNode :: (Ord a, Eq a) => a -> Digraph a -> Bool
isNode n (D d)  =  isJust (lookup n d)

isEdge :: (Ord a, Eq a) => a -> a -> Digraph a -> Bool
isEdge s t (D d)  =  case lookup s d of
                     Just ns -> t `elemOrd` ns
                     Nothing -> False

isPath :: (Ord a, Eq a) => a -> a -> Digraph a -> Bool
isPath s t d  =  t `elemOrd` closeInto d [] [s]

emptyDigraph :: Digraph a
emptyDigraph  =  D []

addNode :: (Ord a, Eq a) => a -> Digraph a -> Digraph a
addNode s (D d)  =
  let (these,those)  =  span ((< s) . fst) d in
  D $ these ++
  case those of
  []            ->  [(s,[])]
  (sd,tsd):etc  ->  if s == sd then error "addNode: already present"
                    else (s,[]) : those

addEdge :: (Ord a, Eq a) => a -> a -> Digraph a -> Digraph a
addEdge s t (D d)  =
  let (these,those)  =  span ((< s) . fst) d in
  D $ these ++
  case those of
  []            ->  [(s,[t])]
  (sd,tsd):etc  ->  if s == sd then
                       if t `elemOrd` tsd then error "addEdge: already present"
                       else (s,insert t tsd) : etc
                    else (s,[t]) : those

-- The function assoc1toNdigraph derives a digraph from an association list
-- pairing single sources with lists of targets.  Sorting is applied to
-- outer and inner lists, so there is no ordering requirement on the argument.
-- If there is more than one pair with the same source, target lists are merged.
-- If the same value appears more than once in a target list, duplicates are removed.
-- If any target does not occur as a source, it is added, with an empty target list.
assoc1toNdigraph :: (Ord a, Eq a) => [(a,[a])] -> Digraph a
assoc1toNdigraph stss  =  D $ addMissingSources $ mergeAndSortTargets $ sort stss
  where
  mergeAndSortTargets []                       =  []
  mergeAndSortTargets [(s,ts)]                 =  [(s, nubOrd $ sort ts)]
  mergeAndSortTargets ((s0,ts0):(s1,ts1):etc)  =
    if s0 == s1 then mergeAndSortTargets ((s0,ts0++ts1):etc)
    else (s0,nubOrd $ sort ts0) : mergeAndSortTargets ((s1,ts1):etc)
  addMissingSources stss  =
    union [(s,[]) | s <- missingSources] stss
    where
    missingSources  =  allTargets `diff` map fst stss
    allTargets      =  foldr union [] (map snd stss)

transitiveClosure :: (Ord a, Eq a) => Digraph a -> Digraph a
transitiveClosure d =  D $ map (close d) (nodeSuccs d)

close :: (Ord a, Eq a) => Digraph a -> (a,[a]) -> (a,[a])
close d (s,ts)  =  (s, closeInto d [] ts)

closeInto :: (Ord a, Eq a) => Digraph a -> [a] -> [a] -> [a]
closeInto d clo []      =  clo
closeInto d clo (t:ts)  =
  case lookup t (nodeSuccs d) of
  Nothing     ->  closeInto d clo ts
  Just tsuccs ->  closeInto d clo' (union (diff tsuccs clo') ts)
  where
  clo'  =  insert t clo

-- auxiliary functions for ordered list processing

nubOrd :: (Ord a, Eq a) => [a] -> [a]
nubOrd []         =  []
nubOrd [x]        =  [x]
nubOrd (x:y:etc)  =  if x==y then nubOrd (y:etc) else x : nubOrd (y:etc)

elemOrd :: Ord a => a -> [a] -> Bool
elemOrd x ys  =  null (diff [x] ys)

insert :: Ord a => a -> [a] -> [a]
insert x ys  =  union [x] ys

union :: Ord a => [a] -> [a] -> [a]
union [] ys         =  ys
union (x:xs) []     =  x:xs
union (x:xs) (y:ys) =  case compare x y of
                       LT -> x : union xs (y:ys)
                       EQ -> union xs (y:ys)
                       GT -> y : union (x:xs) ys

diff :: Ord a => [a] -> [a] -> [a]
diff [] ys          =  []
diff (x:xs) []      =  x:xs
diff (x:xs) (y:ys)  =  case compare x y of
                       LT -> x : diff xs (y:ys)
                       EQ -> diff xs ys
                       GT -> diff (x:xs) ys

-- The result of cycles lists disjoint maximal subsets of nodes in
-- each of which there is a cycle passing through all nodes.
cycles:: (Ord a, Eq a) => Digraph a -> [[a]]
cycles d  =
  let d'          =  transitiveClosure d
      cycleNodes  =  filter (hasLoop d') (sources d')
  in
  map (sources . D) $ groupWith snd $ nodeSuccs $ subgraph cycleNodes d'

hasLoop :: Eq a => Digraph a -> a -> Bool
hasLoop d s  =
  case lookup s (nodeSuccs d) of
  Nothing -> False
  Just ts -> elem s ts

subgraph :: Eq a => [a] -> Digraph a -> Digraph a
subgraph ns d  =  D $ [(s,filter (`elem` ns) ts) | (s,ts) <- nodeSuccs d, elem s ns]

-- The result of topoSort d, where d is an acyclic digraph, lists all nodes
-- in an order where each node precedes all its digraph successors;
-- the result is Nothing if d has a cycle.
topoSort :: (Ord a, Eq a) => Digraph a -> Maybe [a]
topoSort (D [])  =  Just []
topoSort d       =  do
                       guard (not $ null maxima)
                       ns <- topoSort (D nodesuccs')
                       return $ ns ++ maxima
  where
  (these,those)  =  partition (null.snd) (nodeSuccs d)
  maxima         =  nodes (D these)
  nodesuccs'     =  [(s,ts \\ maxima) | (s,ts) <- those]

-- The result of maxDAGfrom s d is a subgraph of d which is a maximal
-- DAG rooted at node s.
maxDagFrom :: (Ord a, Eq a) => a -> Digraph a -> Digraph a
maxDagFrom s d  =  md [] [s] (removeAllEdges d) (removeLoops d)

-- In a call md done todo dag d, done is an ordered list of nodes already
-- visited, todo is a disjoint ordered list of nodes to be visited, dag is
-- the dag so far, and d is the full loop-free digraph.
md :: (Ord a, Eq a) => [a] -> [a] -> Digraph a -> Digraph a -> Digraph a
md _    []     dag d  =  dag
md done (s:ss) dag d  =
  case lookup s (nodeSuccs d) of
  Nothing  ->  md done' ss dag  d
  Just ts  ->  md done' (union (diff ts done) ss) dag' d
               where
               dag'  =  foldr (uncurry addEdgeIfAcyclic) dag [(s,t) | t <- ts]
  where
  done'  =  insert s done

addEdgeIfAcyclic :: (Ord a, Eq a) => a -> a -> Digraph a -> Digraph a
addEdgeIfAcyclic s t d  =  if isPath t s d then d else addEdge s t d

removeLoops :: (Ord a, Eq a) => Digraph a -> Digraph a
removeLoops d  =  D $ [(s, diff ts [s]) | (s, ts) <- nodeSuccs d]

removeAllEdges :: (Ord a, Eq a) => Digraph a -> Digraph a
removeAllEdges d  =  D $ [(s, []) | (s, _) <- nodeSuccs d]

