-- This program applies FitSpec to the Digraph library.
--
-- Usage:
-- analyse properties about membership functions:
--   ./digraphs [options] m
--
-- analyse properties about ispath and subgraph functions:
--   ./digraphs [options] ps <refinement-id>
--
--
-- This program is more complicated than it should, as it:
--   * allows switching between two function-tuples/property-sets -- 'digraphs m' or 'digraphs ps';
--   * allows switching between refinements -- 'digraphs ps 0', 'digraphs ps 1', ...;
--   * uses polymorphism where it could use monomorphism.
import Digraph as D
import Test.FitSpec.Most
import Data.List ((\\))
import qualified Data.List as L (delete)
import Control.Monad

instance (Ord a, Listable a) => Listable (Digraph a) where
  tiers = concatMapT graphs $ setsOf tiers
    where
    graphs ns = mapT (D . zip ns)
              $ listsOfLength (length ns) (setsOf $ toTiers ns)

-- Our digraph instance above is too complicated.
-- Simple reference implementation:
listDigraphsInneficient :: (Ord a, Listable a) => [Digraph a]
listDigraphsInneficient = concat tiers'
  where
  tiers' = cons1 D  `suchThat` okDigraph  `ofWeight` 0

-- Tests Listable Digraph listable instance
-- by comparing to an equivalent inneficient implementation:
--
-- > tiers === tiersDigraphsInneficient
listableOK :: Bool
listableOK = and
  [ holds 10000 $ \d -> okDigraph (d :: Digraph A)                  -- sound
  , take 100 list `subset` (listDigraphsInneficient :: [Digraph A]) -- sound
  , take 100 (listDigraphsInneficient :: [Digraph A]) `subset` list -- complete
  ]
  where xs `subset` ys = all (`elem` ys) xs

-- For debugging the digraphs instance
putDigraphs :: Int -> IO ()
putDigraphs n = putStrLn . unlines . map (unlines . map show)
              $ take n (tiers :: [[Digraph A]])


instance (Ord a, Listable a) => Mutable (Digraph a) where mutiers = mutiersEq

instance (Ord a, Show a, Listable a) => ShowMutable (Digraph a) where
  mutantS = mutantSEq



type Preds a = a -> Digraph a -> [a]
type Succs a = a -> Digraph a -> [a]
type IsNode a = a -> Digraph a -> Bool
type IsEdge a = a -> a -> Digraph a -> Bool

type TyM a = (Preds a, Succs a, IsNode a, IsEdge a)

-- | properties about membership in a digraph
propertiesM :: (Ord a, Eq a, Show a, Listable a) => TyM a -> [Property]
propertiesM (preds, succs, isNode, isEdge) =
  [ property $ \d t   -> D.strictOrder (preds t d)
  , property $ \d s   -> D.strictOrder (succs s d)
  , property $ \d s t -> (s `elem` preds t d) == (t `elem` succs s d)
  , property $ \d s t -> (t `elem` succs s d) == isEdge s t d
  , property $ \d s t -> isEdge s t d ==> (isNode s d && isNode t d)
  , property $ \d s   -> isNode s d == (s `elem` D.nodes d)
  ]

functionsM :: Ord a => TyM a
functionsM = (preds,succs,isNode,isEdge)


type IsPath a = a -> a -> Digraph a -> Bool
type Subgraph a = [a] -> Digraph a -> Digraph a

type TyPS a = (IsPath a, Subgraph a)

-- | properties abouth path and subgraph
propertiesPS :: (Ord a, Eq a, Show a, Listable a) => TyPS a -> [Property]
propertiesPS (isPath, subgraph) =
  [ property $ \n d     -> isPath n n d == isNode n d
  , property $ \n1 n2 n3 d -> isPath n1 n2 d && isPath n2 n3 d ==> isPath n1 n3 d
  , property $ \d -> subgraph (D.nodes d) d == d
  , property $ \ns1 ns2 d -> subgraph ns1 (subgraph ns2 d) == subgraph ns2 (subgraph ns1 d)
  , property $ \n1 n2 ns d -> isPath n1 n2 (subgraph ns d) ==> isPath n1 n2 d

  -- 5-7
  , property $ \n1 n2 d -> isPath n1 n2 d ==> isNode n1 d && isNode n2 d
  , property $ \n1 n2 d -> isPath n1 n2 d && n1 /= n2 ==>
                              any (\n1' -> n1' /= n1 && isPath n1' n2 d) (succs n1 d)
  , property $ \n1 n2 d -> n1 /= n2 ==>
                 isPath n1 n2 d ==
                 let d' = subgraph (nodes d \\ [n1]) d in
                 any (\n1' -> isPath n1' n2 d')
                     (succs n1 d)

  -- 8-9
  , property $ \n ns d -> isNode n (subgraph ns d) == (isNode n d && n `elem` ns)
  , property $ \n1 n2 ns d -> isEdge n1 n2 (subgraph ns d)
            == (isEdge n1 n2 d && n1 `elem` ns && n2 `elem` ns)
  ]

functionsPS :: Ord a => TyPS a
functionsPS = (isPath, subgraph)



extraMutantsM :: Ord a => [TyM a]
extraMutantsM = []

extraMutantsPS :: Ord a => [TyPS a]
extraMutantsPS = drop 1
  [ (isPath, subgraph)
  , (\n1 n2 d -> isNode n1 d && isNode n2 d, subgraph)
  , (isPath, \ns d -> D [])
  ]

-- Choosen node-label test type.
type A = Nat

main = do
  unless listableOK
    $ putStrLn "WARNING: Listable Digraph is broken!  (read my source.)"

  as <- getArgsWith args { names = [ "isPath n1 n2 d"
                                   , "subgraph ns d" ] }

  let (pset,step) = case extra as of
                      (p:s:_) -> (p  ,read s)
                      [p]     -> (p  ,0)
                      []      -> ("m",0)

  case pset of
    "m" ->
      mainWith as { names = [ "preds n d"
                            , "succs n d"
                            , "isNode n d"
                            , "isEdge s t d" ] }
                  (functionsM :: TyM A)
                  propertiesM

    "ps" ->
      let is = case step of
                 0 -> [                   ]
                 1 -> [0,1,2,3,4          ]
                 2 -> [0,1,2,3,4,5,6      ]
                 3 -> [0,1,2,3,4,5,6,  8,9]
                 4 -> [0,1,2,3,4,5,  7,8,9]
                 5 -> [0,            7,8,9]
                 _ -> [0,1,2,3,4,5,6,7,8,9] -- not an actual step
      in
      mainWith as { names = [ "isPath n1 n2 d"
                            , "subgraph ns d" ] }
                  (functionsPS :: TyPS A)
                  ((!!! is) . propertiesPS)

(!!!) :: [a] -> [Int] -> [a]
xs !!! is = map (xs !!) is
