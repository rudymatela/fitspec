{-# LANGUAGE CPP #-}
import Test.FitSpec
import Text.PrettyPrint as P

#if __GLASGOW_HASKELL__ < 710
-- pretty <= 1.1.1.1 (bundled with GHC <= 7.8)  does not provide this instance
-- pretty >= 1.1.2.0 (bundled with GHC >= 7.10) does provide this instance
instance Eq Doc where
  d == d' = show d == show d'
#endif

instance Listable Doc where
  tiers = cons1 text  -- TODO: Improve this

instance Mutable Doc where
  mutiers = mutiersEq

instance ShowMutable Doc where
  mutantS = mutantSEq

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
  as <- getArgs
  let run ps = reportWith as ((P.<>),($$),nest) (uncurry3 ps)
  case concat (extra as) of
    "qs"    -> run propertiesQS
    ""      -> run properties

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z
