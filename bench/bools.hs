import FitSpec
import Test.Check


propertiesN :: (Bool -> Bool) -> [Property]
propertiesN not =
  [ property $ \p -> not (not p) == p
  , property $ \p -> not p /= p
  , property $       not True == False
  ]

propertiesNA :: (Bool -> Bool) -> (Bool -> Bool -> Bool) -> [Property]
propertiesNA not (&&) =
  [ property $ \p     -> not (not p)     == p
  , property $ \p q   -> p && q          == q && p
  , property $ \p     -> p && p          == p
  , property $ \p     -> p && False      == False
  , property $ \p q r -> p && (q && r)   == (p && q) && r
  , property $ \p     -> p && not p      == False
  , property $ \p     -> p && not False  == p
  ]

propertiesNAO :: (Bool->Bool) -> (Bool->Bool->Bool) -> (Bool->Bool->Bool)
              -> [Property]
propertiesNAO not (&&) (||) =
  [ property $                not True == False
  , property $ \p     ->   not (not p) == p

  , property $ \p q   ->        p && q == q && p
  , property $ \p     ->        p && p == p
  , property $ \p     ->     p && True == p
  , property $ \p     ->    p && False == False
  , property $ \p q r -> p && (q && r) == q && (p && r)

  , property $ \p q   ->        p || q == q || p
  , property $ \p     ->        p || p == p
  , property $ \p     ->     p || True == True
  , property $ \p     ->    p || False == p
  , property $ \p q r -> p || (q || r) == q || (p || r)

  , property $ \p q   -> p && (p || q) == p
  , property $ \p q   -> p || (p && q) == p
  , property $ \p     ->    p && not p == False
  ]

main = do putStrLn "### Strict mutant enumerations ###"

          putStrLn "Not:";
          reportWith args { callNames = ["not p"]
                          , nTestsF = (*100) }
                     not propertiesN

          putStrLn "Not, and:"
          reportWith args { limitResults = Just 9
                          , nTestsF = (*100)
                          , callNames = ["not p","p && q"] }
                     (not,(&&)) (uncurry propertiesNA)

          {-
          putStrLn "Not, and, or:"
          reportWith args { limitResults = Just 2
                          , showPropertySets = unlines
                          , nTestsF = (*100)
                          , callNames = ["not p","p && q","p && q"] }
                     (not,(&&),(||)) (uncurry3 propertiesNAO)
          -- -}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z
