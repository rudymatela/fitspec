import Data.List
import Test.FitSpec


ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [_] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

permutation :: Eq a => [a] -> [a] -> Bool
[]     `permutation` []    = True
(_:_)  `permutation` []    = False
[]     `permutation` (_:_) = False
(x:xs) `permutation` ys    = x `elem` ys  &&  xs `permutation` delete x ys

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)


properties :: (Ord a, Show a, Listable a)
           => ([a] -> [a]) -> [Property]
properties sort =
  [ property $ \xs ->          ordered (sort xs)
  , property $ \xs ->           length (sort xs) == length xs
  , property $ \x xs ->         elem x (sort xs) == elem x xs
  , property $ \x xs ->        count x (sort xs) == count x xs
  , property $ \xs ->   permutation xs (sort xs)
  , property $ \x xs ->       insert x (sort xs) == sort (x:xs)
  ]


sargs :: Args
sargs = args
  { names = ["sort xs"]
  , timeout  =    0
  , nMutants = 1000
  , nTests   = 1000
  }
--, extraMutants = take 0
--               . concat
--               . lsmap (. sort)
--               $ cons2 (\y ys -> (++ (y:ys))) -- prepend non-empty list
--            \++/ cons2 (\y ys -> ((y:ys) ++)) -- append non-empty list

type Ty a = [a] -> [a]

main :: IO ()
main = do
  as <- getArgsWith sargs
  let run f = reportWith as f properties
  case concat $ extra as of
    "bool"  -> run (sort :: Ty Bool)
    "bools" -> run (sort :: Ty [Bool])
    "int"   -> run (sort :: Ty Int)
    "i1"    -> run (sort :: Ty Int1)
    "i2"    -> run (sort :: Ty Int2)
    "i3"    -> run (sort :: Ty Int3)
    "w1"    -> run (sort :: Ty Word1)
    "w2"    -> run (sort :: Ty Word2)
    "w3"    -> run (sort :: Ty Word3)
    "unit"  -> run (sort :: Ty ())
    ""      -> run (sort :: Ty Word2)
    t       -> putStrLn $ "unknown type " ++ t

-- This hack is only necessary when using sortCounter as a manual mutant
instance Bounded a => Bounded [a] where
  minBound = []
  maxBound = repeat maxBound -- non terminating upper bound

sortCounter :: (Bounded a, Ord a) => [a] -> [a]
sortCounter = (++ [maxBound]) . sort
