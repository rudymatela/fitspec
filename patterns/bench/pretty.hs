module Main where
import Text.PrettyPrint
import Mutants hiding (($$))

instance Enumerable (Doc) where
  enumerate = datatype [c1 text]

instance Parameter (Doc) where
  functions = lets "splitP" splitP

-- Splits into first line, then other lines.
-- If there is just one line, splits into first char, other chars
-- Otherwise does not split
-- NOTE: This implementation assumes that documents are always
-- terminated in newlines when rendered
splitP :: Doc -> Maybe (Doc,Doc)
splitP d = case lines (render d) of
             []     -> Nothing
             [""]   -> Nothing
             [c:cs] -> Just (char c, text cs)
             (l:ls) -> Just (text l, text (unlines ls))

propertyMap :: Int -> ((Doc,Doc) -> Doc) -> ((Doc,Doc) -> Doc) -> ((Int,Doc) -> Doc) -> [Bool]
propertyMap n beside' above' nest'' =
  [ runTests n $ \(x,y,z) ->       (x <> y) <> z == x <> (y <> z)
  , runTests n $ \(x,y,z) ->       (x $$ y) $$ z == x $$ (y $$ z)
  , runTests n $ \x ->              x <> text "" == x
  , runTests n $ \(x,k,y) ->     nest k (x $$ y) == nest k x $$ nest k y
  , runTests n $ \(x,k,y) ->     nest k (x <> y) == nest k x <> y
  , runTests n $ \(x,k,y) ->       x <> nest k y == x <> y
  , runTests n $ \(x,k,k') -> nest k (nest k' x) == (nest (k+k') x)
  , runTests n $ \x ->                  nest 0 x == x
  , runTests n $ \(x,y,z) ->       (x $$ y) <> z == x $$ (y <> z)
  , runTests n $ \(s,y,z) -> text s <> ((text "" <> y) $$ z) == (text s <> y) $$ nest (length s) z
  , runTests n $ \(s,t) ->      text s <> text t == text (s ++ t)
  ]
  where (<>) = curry beside'
        ($$) = curry above'
        nest = curry nest''

propertyMapQS :: Int -> ((Doc,Doc) -> Doc) -> ((Doc,Doc) -> Doc) -> ((Int,Doc) -> Doc) -> [Bool]
propertyMapQS n beside' above' nest'' =
  [ runTests n $ \(x,y,z) ->       (x <> y) <> z == x <> (y <> z)
  , runTests n $ \(x,y,z) ->       (x $$ y) $$ z == x $$ (y $$ z)
  , runTests n $ \(x,y,z) ->       (x $$ y) <> z == x $$ (y <> z)
  , runTests n $ \(x,k,y) ->       x <> nest k y == x <> y
  , runTests n $ \(x,k,y) ->     nest k (x <> y) == nest k x <> y
  , runTests n $ \(x,k,y) ->     nest k (x $$ y) == nest k x $$ nest k y
  , runTests n $ \(x,k,k') -> nest k (nest k' x) == nest k' (nest k x)
  ]
  where (<>) = curry beside'
        ($$) = curry above'
        nest = curry nest''

propertyMap' :: Int
             -> [((Doc, Doc) -> Doc
                 ,((Doc, Doc) -> Doc
                  ,(Int, Doc) -> Doc))
                -> Bool]
propertyMap' = map uncurry3' . transform3 . propertyMap

main = do runV (Just $ valid 4000)
               9
               (propertyMap' $ 4000)
               (uncurry (<>),(uncurry ($$),uncurry nest))

valid :: Int
      -> ((Doc,Doc) -> Doc
         ,((Doc,Doc) -> Doc
          ,(Int,Doc) -> Doc))
      -> ()
valid n (f,(g,h)) = v f `seq` v g `seq` v h where
  v a = runTests n ((`seq` True) . a) `seq` ()

uncurry3' :: (a -> b -> c -> d) -> (a,(b,c)) -> d
uncurry3' f = \(x,(y,z)) -> f x y z

transform3 :: (a -> b -> c -> [d]) -> [a -> b -> c -> d]
transform3 f = f' <$> [0 .. length (f undefined undefined undefined) - 1]
  where f' k x y z = f x y z !! k
