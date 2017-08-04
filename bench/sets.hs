import Set as S
import Test.FitSpec hiding ((\/),Set)


instance (Ord a, Listable a) => Listable (S.Set a) where
  tiers = setCons set

instance (Ord a, Listable a) => Mutable (S.Set a) where
  mutiers = mutiersEq

instance (Ord a, Show a, Listable a) => ShowMutable (S.Set a) where
  mutantS = mutantSEq


type Elem a = a -> Set a -> Bool


-- The property map.
-- In a real program applying FitSpec, the 'Int' parameter would not exist.
-- It is here so we can re-run the steps taken in creating the final
-- property list (by @properties 3@).
properties 0 ((<~), insertS, deleteS, (/\), (\/), subS) =
  [
  ]
properties 1 ((<~), insertS, deleteS, (/\), (\/), subS) =
  [ property $ \x s -> x <~ insertS x s
  , property $ \x s -> not (x <~ deleteS x s)
  , property $ \x s t -> (x <~ (s \/ t)) == ((x <~ s) || (x <~ t))
  , property $ \x s t -> (x <~ (s /\ t)) == ((x <~ s) && (x <~ t))
  , property $ \s t -> subS s (s \/ t)
  , property $ \s t -> subS (s /\ t) s
  , property $ \s t -> (s \/ t) == (t \/ s)
  , property $ \s t -> (s /\ t) == (t /\ s)
  ]
properties 2 ((<~), insertS, deleteS, (/\), (\/), subS) =
  [ property $ \x s -> x <~ insertS x s
  , property $ \x s -> not (x <~ deleteS x s)
  , property $ \x s t -> (x <~ (s \/ t)) == ((x <~ s) || (x <~ t))
  , property $ \x s t -> (x <~ (s /\ t)) == ((x <~ s) && (x <~ t))
  , property $ \s t -> subS s t == allS (<~ t) s
  ]
properties _ ((<~), insertS, deleteS, (/\), (\/), subS) =
  [ property $ \x y s -> x <~ insertS y s == (x == y || x <~ s)
  , property $ \x y s -> x <~ deleteS y s == (x <~ s && x /= y)
  , property $ \x s t -> (x <~ (s \/ t)) == ((x <~ s) || (x <~ t))
  , property $ \x s t -> (x <~ (s /\ t)) == ((x <~ s) && (x <~ t))
  , property $ \s t -> subS s t == allS (<~ t) s
  ]
-- If we add (\\) to the functions under test, this property should follow:
-- \x s t -> (x <~ (s \\ t)) == ((x <~ s) && not (x <~ t))


main :: IO ()
main = do
  as <- getArgs
  let psid = case concat . extra $ as of "" -> 0; cs -> read cs
  mainWith as { names = [ "x <~ s"
                        , "insertS x s"
                        , "deleteS x s"
                        , "s /\\ t"
                        , "s \\/ t"
                        , "subS s t" ] }
           ((<~)::Elem Word2, insertS, deleteS, (/\), (\/), subS)
           (properties psid)
