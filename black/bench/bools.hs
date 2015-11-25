import FitSpec
import FitSpecC
import Test.Check


pMapN :: Int -> (Bool -> Bool) -> [Bool]
pMapN n not =
  [ holds n $ \p -> not (not p) == p
  , holds n $ \p -> not p /= p
  ,                 not True == False
  ]

pMapNA :: Int -> (Bool -> Bool) -> (Bool -> Bool -> Bool) -> [Bool]
pMapNA n not (&&) =
  [ holds n $ \p     -> not (not p)     == p
  , holds n $ \p q   -> p && q          == q && p
  , holds n $ \p     -> p && p          == p
  , holds n $ \p     -> p && False      == False
  , holds n $ \p q r -> p && (q && r)   == (p && q) && r
  , holds n $ \p     -> p && not p      == False
  , holds n $ \p     -> p && not False  == p
  ]

pMapNAO :: Int
        -> (Bool->Bool) -> (Bool->Bool->Bool) -> (Bool->Bool->Bool)
        -> [Bool]
pMapNAO n not (&&) (||) =
  [ holds n $                not True == False
  , holds n $ \p     ->   not (not p) == p

  , holds n $ \p q   ->        p && q == q && p
  , holds n $ \p     ->        p && p == p
  , holds n $ \p     ->     p && True == p
  , holds n $ \p     ->    p && False == False
  , holds n $ \p q r -> p && (q && r) == q && (p && r)

  , holds n $ \p q   ->        p || q == q || p
  , holds n $ \p     ->        p || p == p
  , holds n $ \p     ->     p || True == True
  , holds n $ \p     ->    p || False == p
  , holds n $ \p q r -> p || (q || r) == q || (p || r)

  , holds n $ \p q   -> p && (p || q) == p
  , holds n $ \p q   -> p || (p && q) == p
  , holds n $ \p     ->    p && not p == False
  ]

main = do putStrLn "### Strict mutant enumerations ###"

          putStrLn "Not:";
          reportWith args { callNames = ["not p"] }
                     not pMapN

          putStrLn "Not, and:"
          reportWith args { limitResults = Just 9
                          , callNames = ["not p","p && q"] }
                     (not,(&&)) (uncurry . pMapNA)

          {-
          putStrLn "Not, and, or:"
          reportWith args { limitResults = Just 2
                          , showPropertySets = unlines
                          , callNames = ["not p","p && q","p && q"] }
                     (not,(&&),(||)) (uncurry3 . pMapNAO)
          -- -}

          putStrLn "Not, and (filtered):"
          report2 500 not (uncurry (&&)) (upmap 500)
            where upmap n f g = pMapNA n f (curry g)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z
