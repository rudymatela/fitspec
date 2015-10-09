import FitSpec
import FitSpecC
import Test.Check
import Mutate
import Text.PrettyPrint

instance Listable Doc where
  listing = cons1 text  -- TODO: Improve this

instance Mutable Doc where
  szMutants = lsMutantsEq

instance ShowMutable Doc where
  showMutant = showMutantEq

propertyMap :: Int -> ((Doc,Doc) -> Doc) -> ((Doc,Doc) -> Doc) -> ((Int,Doc) -> Doc) -> [Bool]
propertyMap n beside' above' nest'' =
  [ holds n $ \x y z ->       (x <> y) <> z == x <> (y <> z)
  , holds n $ \x y z ->       (x $$ y) $$ z == x $$ (y $$ z)
  , holds n $ \x ->            x <> text "" == x
  , holds n $ \x k y ->     nest k (x $$ y) == nest k x $$ nest k y
  , holds n $ \x k y ->     nest k (x <> y) == nest k x <> y
  , holds n $ \x k y ->       x <> nest k y == x <> y
  , holds n $ \x k k' -> nest k (nest k' x) == (nest (k+k') x)
  , holds n $ \x ->                nest 0 x == x
  , holds n $ \x y z ->       (x $$ y) <> z == x $$ (y <> z)
  , holds n $ \s y z -> text s <> ((text "" <> y) $$ z) == (text s <> y) $$ nest (length s) z
  , holds n $ \s t ->      text s <> text t == text (s ++ t)
  ]
  where (<>) = curry beside'
        ($$) = curry above'
        nest = curry nest''

propertyMapQS :: Int -> ((Doc,Doc) -> Doc) -> ((Doc,Doc) -> Doc) -> ((Int,Doc) -> Doc) -> [Bool]
propertyMapQS n beside' above' nest'' =
  [ holds n $ \x y z ->       (x <> y) <> z == x <> (y <> z)
  , holds n $ \x y z ->       (x $$ y) $$ z == x $$ (y $$ z)
  , holds n $ \x y z ->       (x $$ y) <> z == x $$ (y <> z)
  , holds n $ \x k y ->       x <> nest k y == x <> y
  , holds n $ \x k y ->     nest k (x <> y) == nest k x <> y
  , holds n $ \x k y ->     nest k (x $$ y) == nest k x $$ nest k y
  , holds n $ \x k k' -> nest k (nest k' x) == nest k' (nest k x)
  ]
  where (<>) = curry beside'
        ($$) = curry above'
        nest = curry nest''

main = do reportWith args { limitResults = Just 20 }
                     1000 (uncurry (<>),uncurry ($$),uncurry (nest)) (pmap 2000)
          -- 2000m x 4000t x 7laws: 1m5s
          reportWith args { limitResults = Just 20 }
                     1000 (uncurry (<>),uncurry ($$),uncurry (nest)) (pmapQS 2000)
  where pmap = uncurry3 . propertyMap
        pmapQS = uncurry3 . propertyMapQS

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z
