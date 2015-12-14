import FitSpec.Main
import Test.Check
import Mutate
import Mutate.Show
import Text.PrettyPrint

instance Listable Doc where
  listing = cons1 text  -- TODO: Improve this

instance Mutable Doc where
  lsMutants = lsMutantsEq

instance ShowMutable Doc where
  mutantS = mutantSEq
  showMutant _ = show

properties :: (Doc->Doc->Doc) -> (Doc->Doc->Doc) -> (Int->Doc->Doc) -> [Property]
properties (<>) ($$) nest =
  [ property $ \x y z ->       (x <> y) <> z == x <> (y <> z)
  , property $ \x y z ->       (x $$ y) $$ z == x $$ (y $$ z)
  , property $ \x ->            x <> text "" == x
  , property $ \x k y ->     nest k (x $$ y) == nest k x $$ nest k y
  , property $ \x k y ->     nest k (x <> y) == nest k x <> y
  , property $ \x k y ->       x <> nest k y == x <> y
  , property $ \x k k' -> nest k (nest k' x) == nest (k+k') x
  , property $ \x ->                nest 0 x == x
  , property $ \x y z ->       (x $$ y) <> z == x $$ (y <> z)
  , property $ \s y z -> text s <> ((text "" <> y) $$ z) == (text s <> y) $$ nest (length s) z
  , property $ \s t ->      text s <> text t == text (s ++ t)
  ]

propertiesQS :: (Doc->Doc->Doc) -> (Doc->Doc->Doc) -> (Int->Doc->Doc) -> [Property]
propertiesQS (<>) ($$) nest =
  [ property $ \x y z ->       (x <> y) <> z == x <> (y <> z)
  , property $ \x y z ->       (x $$ y) $$ z == x $$ (y $$ z)
  , property $ \x y z ->       (x $$ y) <> z == x $$ (y <> z)
  , property $ \x k y ->       x <> nest k y == x <> y
  , property $ \x k y ->     nest k (x <> y) == nest k x <> y
  , property $ \x k y ->     nest k (x $$ y) == nest k x $$ nest k y
  , property $ \x k k' -> nest k (nest k' x) == nest k' (nest k x)
  ]

main = do
  let run ps = defaultMain ((<>),($$),nest) (uncurry3 ps)
  ty <- typeArgument
  case ty of
    "qs"    -> run propertiesQS
    _       -> run properties

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z
