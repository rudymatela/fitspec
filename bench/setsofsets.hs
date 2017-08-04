import Set as S
import Test.FitSpec hiding ((\/),Set)


instance (Ord a, Listable a) => Listable (S.Set a) where
  tiers = setCons set

instance (Ord a, Listable a) => Mutable (S.Set a) where
  mutiers = mutiersEq

instance (Ord a, Show a, Listable a) => ShowMutable (S.Set a) where
  mutantS = mutantSEq

-- The Mutable and ShowMutable instances could be alternatively derived by:
--   deriveMutable 'Set


-- Type of functions under test
type PowerS a      = S.Set a -> S.Set (S.Set a)
type PartitionsS a = S.Set a -> S.Set (S.Set (S.Set a))
type Ty a = (PowerS a, PartitionsS a)


-- The property map.
-- In a real program applying FitSpec, the 'Int' parameter would not exist.
-- It is here so we can re-run the steps taken in creating the final
-- property list (by @properties 5@).
properties :: (Ord a, Eq a, Show a, Listable a)
           => Int -> Ty a -> [Property]
properties 0 (powerS,partitionsS) =
  [
  ]
properties 1 (powerS,partitionsS) =
  [ property $ \s t -> (t <~ powerS s) == subS t s
  , property $ \s -> allS (allS (`subS` s)) (partitionsS s)
  ]
properties 2 (powerS,partitionsS) =
  [ property $ \s t -> (t <~ powerS s) == subS t s
  , property $ \s -> allS (allS (`subS` s)) (partitionsS s)
  , property $ \s -> nonEmptyS (partitionsS s)
  ]
properties 3 (powerS,partitionsS) =
  [ property $ \s t -> (t <~ powerS s) == subS t s
  , property $ \s -> allS (allS (`subS` s)) (partitionsS s)
  , property $ \s -> nonEmptyS (partitionsS s)
  , property $ \s -> allS (allS (\t -> nonEmptyS t && subS t s)) (partitionsS s)
  ]
-- Ommited on paper -- we changed 4th to:
--, property $ \s -> allS (\p -> unionS p == s && allS nonEmptyS p) (partitionsS s)
properties _ (powerS,partitionsS) =
  [ property $ \s t -> (t <~ powerS s) == subS t s
  , property $ \s p -> (p <~ partitionsS s) ==
                       ( unionS p == s &&
                         allS nonEmptyS p &&
                         sum (map sizeS (elemList p)) == sizeS s )
  ]


fns :: Ord a => Ty a
fns = (powerS, partitionsS)

main :: IO ()
main = do
  as <- getArgs
  let psid = read . concat . extra $ as
  mainWith as { names = [ "powerS s"
                        , "partitionsS s" ] }
           (fns::Ty Word2)
           (properties psid)
