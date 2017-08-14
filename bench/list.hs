import Test.FitSpec
import Data.List

type Cons a = a -> [a] -> [a]
type Head a = [a] -> a
type Tail a = [a] -> [a]
type Append a = [a] -> [a] -> [a]
type Ty a = ( Cons a
            , Head a
            , Tail a
            , Append a
            )

-- The property map
properties :: (Eq a, Show a, Listable a)
           => Cons a
           -> Head a
           -> Tail a
           -> Append a
           -> [Property]
properties (-:) head tail (++) =
  [ property $ \xs -> [] ++ xs == xs && xs == xs ++ []
  , property $ \x xs -> head (x-:xs) == x
  , property $ \x xs -> tail (x-:xs) == xs
  , property $ \xs -> null (xs ++ xs) == null xs
  , property $ \xs ys zs -> (xs ++ ys) ++ zs == xs ++ (ys ++ zs)
  , property $ \x xs ys -> x-:(xs ++ ys) == (x-:xs) ++ ys
  ]


fns :: Ty a
fns = ((:),head,tail,(++))

sargs :: Args
sargs = args
  { names = ["(:) x xs","head xs","tail xs","(++) xs ys"]
  , nMutants = 1000
  , nTests   = 1000
  , timeout  = 0
  }

--, extraMutants = takeWhile (const False)
--               [ ((:),head,tail,(++-))
--               , ((:),head,tail,(++--))
--               ]

main :: IO ()
main = do 
  as <- getArgsWith sargs
  let run f = reportWith as f (uncurry4 properties)
  case concat (extra as) of
    "bool"  -> run (fns :: Ty Bool)
    "bools" -> run (fns :: Ty [Bool])
    "int"   -> run (fns :: Ty Int)
    "int2"  -> run (fns :: Ty UInt2)
    "int3"  -> run (fns :: Ty UInt3)
    "unit"  -> run (fns :: Ty ())
    ""      -> run (fns :: Ty UInt2)

-- Some manual mutants
(++-) :: [a] -> [a] -> [a]
xs ++- ys = []

(++--) :: [a] -> [a] -> [a]
xs ++-- ys = if length xs > length ys
               then xs
               else ys

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (x,y,z,w) = f x y z w

