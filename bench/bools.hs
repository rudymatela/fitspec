import FitSpec

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

main = do
  as <- getArgsWith args { names = ["not p","p && q","p || q"]
                         , nMutants = 100
                         , nTests   = 100
                         , timeout  = 0 }
  let run f ps = reportWith as f ps
  case concat $ extra as of
    "nao" -> run (not,(&&),(||)) (uncurry3 propertiesNAO)
    "na"  -> run (not,(&&))      (uncurry  propertiesNA)
    _     -> run  not                      propertiesN

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z
