-- | A very simple data structure for sets of small ints.
module BitMask 
  ( BitSet
  , member, emp, full, insert, delete, union, intersect, excluding, subset, size -- These are all constant time
  , unions, intersects
  , toList, fromList, toList'
  , findComplete
  ) where

import Data.Bits
-- import Data.Monoid
import Data.List hiding (insert, union, delete, intersect)
import Data.Ord

type BitSet = Int


emp :: BitSet
emp = 0

full = complement emp

insert, delete :: Int -> BitSet -> BitSet
insert = flip setBit
delete = flip clearBit

union, intersect, excluding :: BitSet -> BitSet -> BitSet
union      = (.|.)
intersect  = (.&.)
x `excluding` y = x .&. complement y

unions, intersects :: [BitSet] -> BitSet
unions = foldr union emp

intersects = foldr intersect full

member :: Int -> BitSet -> Bool
member = flip testBit

subset :: BitSet -> BitSet -> Bool
subset s t = s == s .&. t

size :: BitSet -> Int
size = popCount


toList' :: Int -> BitSet -> [Int]
toList' k s = [x | x <- [0..k], testBit s x]

fromList :: [Int]-> BitSet
fromList = foldr insert emp

toList = toList' maxBits
maxBits = finiteBitSize (0 :: Int)

-- Finds all minimal complete k-combinations given a list of survivors. Optimised for small memory footprint
findComplete :: Int -> [BitSet] -> [BitSet]
findComplete k ss = filter minim $ complete (0,k) emp ss
  where
    minim x = minComplete ss x == x

complete :: (Int,Int) -> BitSet -> [BitSet] -> [BitSet]
complete (lo,hi) cur xs 
  | null xs    = [cur]
  | otherwise  = concat rec
  where 
    options = [lo+1..hi]
    rec = [ complete (b,hi) (insert b cur) (filter (member b) xs) | b <- options]

run' k ss = map toList $ findComplete k (map fromList ss)

-- | Given a set of survivors and a complete set s, returns the minimal complete subset of s. This should work at least when removing the greatest element in s makes it incomplete. 
minComplete :: [BitSet] -> BitSet -> BitSet
minComplete xs s = unions (filter ((==1) . size) [s `excluding` x | x <- xs ])


-- Testing against a reference implementation
prop (h,t) = findComplete k xs == run2' k xs
  where  k = 5
         xs = map (delete 0) $ map abs $ h:t

pow []      = [[]]
pow (x:xs)  = pow xs ++ map (x:) (pow xs)

run2 k ns = sort $ go (pow [1..k]) where
  go [] = []
  go (x:xs) | minim x    = x : filter (not . (x `subset'`)) (go xs)
            | otherwise  = go xs
  minim x = not (any (x `subset'`) ns)
                                        
subset' xs ys = xs \\ ys == []

run2' k xs = map (fromList . (filter (<= k))) (run2 k (map (toList' k) xs))


  

{- This does not work
run3 k ns0 = concatMap start [1..k] where
  ns = maxsets (sort ns0)
  start n = go n emp ns
  go :: Int -> BitSet -> [BitSet] -> [BitSet]
  go n alts excl = case prt n excl of
    ([],d) -> [insert n emp]
    (_,[]) -> []
    (r,d)  -> map (insert n) $ concat [go x alts' r | x <- [n+1..k], x `member` alts']
      where alts' = intersects (alts:d)

prt x = partition (member x)



-- Input needs to be ordered by size in descending order
maxsets :: [BitSet] -> [BitSet]
maxsets []      = [] 
maxsets (x:xs)  = if any (x `subset`) xs then maxsets xs else x:maxsets xs
-}
