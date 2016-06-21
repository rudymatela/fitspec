-- A list-based library for programming with sets.
-- Colin Runciman, June 2007 to April 2008.

module Set (Set, elemList, set, emptyS, singleS, pairS, insertS, deleteS, 
            sizeS, sizeAtMostS, sizeExactlyS, sizeAtLeastS,
            isEmptyS, nonEmptyS, minS, choiceS, (<~),
            (\/), (/\), (\\), unionS, interS, subS, disjointS,
            elemSubsetsOf, powerS, partitionsS, subsetPartitionsS,
            (<|), allS, anyS, exactly, forAll, thereExists, forExactly,
            minimalS, mapS, mapMonoS, unionMapS, regular) where

import Data.List (nub, sort, intersperse)

infixl 7 /\
infixl 6 \/
infixr 5 `elemSubsetsOf`, `subsetPartitionsS`, <|
infix  4 <~, `subS`

data Set a = S {elemList :: [a]}

instance (Ord a, Eq a) => Eq (Set a)
  where
  S xs == S ys = xs == ys

instance Ord a => Ord (Set a)
  where
  compare (S xs) (S ys) = compare xs ys

instance (Ord a, Show a) => Show (Set a)
  where
  show (S xs) =
    "{"++concat (intersperse "," (map show xs))++"}"

set :: Ord a => [a] -> Set a
set = S . nub . sort

emptyS :: Ord a => Set a
emptyS = S []

singleS :: Ord a => a -> Set a
singleS e = S [e]

pairS :: Ord a => a -> a -> Set a
pairS e1 e2 = set [e1,e2]

insertS :: Ord a => a -> Set a -> Set a
insertS e = S . insertList e . elemList
  where
  insertList e []         = [e]
  insertList e xs@(x:xs') = case compare e x of
                            LT -> e : xs
                            EQ -> xs
                            GT -> x : insertList e xs'

deleteS :: Ord a => a -> Set a -> Set a
deleteS e = S . deleteList e . elemList
  where
  deleteList e []         = []
  deleteList e xs@(x:xs') = case compare e x of
                            LT -> xs
                            EQ -> xs'
                            GT -> x : deleteList e xs'

sizeS :: Ord a => Set a -> Int
sizeS = length . elemList

sizeExactlyS :: Ord a => Int -> Set a -> Bool
sizeExactlyS n = lengthExactly n . elemList
  where
  lengthExactly 0 xs     = null xs
  lengthExactly n []     = False
  lengthExactly n (x:xs) = lengthExactly (n-1) xs

sizeAtLeastS :: Ord a => Int -> Set a -> Bool
sizeAtLeastS n = lengthAtLeast n . elemList
  where
  lengthAtLeast 0 xs     = True
  lengthAtLeast n []     = False
  lengthAtLeast n (x:xs) = lengthAtLeast (n-1) xs

sizeAtMostS :: Ord a => Int -> Set a -> Bool
sizeAtMostS n = lengthAtMost n . elemList
  where
  lengthAtMost 0 xs     = null xs
  lengthAtMost n []     = True
  lengthAtMost n (x:xs) = lengthAtMost (n-1) xs

isEmptyS :: Ord a => Set a -> Bool
isEmptyS = null . elemList

nonEmptyS :: Ord a => Set a -> Bool
nonEmptyS = not . isEmptyS

minS :: Ord a => Set a -> a
minS = head . elemList

choiceS :: Ord a => Set a -> Set (a, Set a)
choiceS = S . choice . elemList
  where
  choice xs = [(x, S (xs1++xs2)) | (xs1,x:xs2) <- splits xs]  

splits :: [a] -> [([a],[a])]
splits []     = [([],[])]
splits (x:xs) = ([],x:xs) : [(x:xs1, xs2) | (xs1,xs2) <- splits xs]

(<~) :: Ord a => a -> Set a -> Bool
(<~) e = ordElem e . elemList
  where
  ordElem e []     = False
  ordElem e (x:xs) = case compare e x of
                     LT -> False
                     EQ -> True
                     GT -> ordElem e xs

(\/) :: Ord a => Set a -> Set a -> Set a
S xs \/ S ys = S (join xs ys)
  where
  join [] ys = ys
  join xs [] = xs
  join xs@(x:xs') ys@(y:ys') =
    case compare x y of
    LT -> x : join xs' ys
    EQ -> x : join xs' ys'
    GT -> y : join xs  ys'

(/\) :: Ord a => Set a -> Set a -> Set a
S xs /\ S ys = S (meet xs ys)

meet [] _  = []
meet _  [] = []
meet xs@(x:xs') ys@(y:ys') =
  case compare x y of
  LT ->     meet xs' ys
  EQ -> x : meet xs' ys'
  GT ->     meet xs  ys'

(\\) :: Ord a => Set a -> Set a -> Set a
S xs \\ S ys = S (diff xs ys)

diff [] _  = []
diff xs [] = xs
diff xs@(x:xs') ys@(y:ys') =
  case compare x y of
  LT -> x : diff xs' ys
  EQ ->     diff xs' ys'
  GT ->     diff xs  ys'

unionS :: Ord a => Set (Set a) -> Set a
unionS = foldr (\/) emptyS . elemList

interS :: Ord a => Set (Set a) -> Set a
interS = foldr1 (/\) . elemList

disjointS :: Ord a => Set a -> Set a -> Bool
disjointS (S xs) (S ys) = null (meet xs ys)

subS :: Ord a => Set a -> Set a -> Bool
subS (S xs) (S ys) = null (diff xs ys)

elemSubsetsOf :: Ord a => Int -> Set a -> Set (Set a)
elemSubsetsOf n =
  S . map S . sublistsOf n . elemList
  where
  sublistsOf 0 _      = [[]]
  sublistsOf _ []     = []
  sublistsOf n (x:xs) =
    map (x:) (sublistsOf (n-1) xs) ++ sublistsOf n xs

powerS :: Ord a => Set a -> Set (Set a)
powerS = 
  S . map S . ([]:) . nonEmptySublists . elemList
  where
  nonEmptySublists [] = []
  nonEmptySublists (x:xs) =
    [x] : map (x:) ss ++ ss
    where
    ss = nonEmptySublists xs

-- outer 'set' used to be 'S' but then ordering between
-- partitions can be wrong
-- TO DO: instead reorder partitionsList computation?
partitionsS :: Ord a => Set a -> Set (Set (Set a))
partitionsS = set . map (S . map S) . partitionsList . elemList
  where
  partitionsList []     = [[]]
  partitionsList (x:xs) =
    [[x] : p | p <- ps] ++
    [(x:xs') : xss ++ xss' | p <- ps, (xss,xs':xss') <- splits p]
    where
    ps = partitionsList xs

subsetPartitionsS :: Ord a => Int -> Set a -> Set (Set (Set a))
subsetPartitionsS n = S . map (S . map S) . sublistPartitionsList n . elemList
  where
  sublistPartitionsList n []  = [[] | n == 0]
  sublistPartitionsList n (x:xs) =
    [ [x] : p
    | n > 0, p <- sublistPartitionsList (n-1) xs ] ++
    [ (x:xs') : xss ++ xss'
    | n > 0, p <- sublistPartitionsList n xs, (xss,xs':xss') <- splits p ]

(<|) :: Ord a => (a -> Bool) -> Set a -> Set a
(<|) p = S . filter p . elemList

allS, anyS :: Ord a => (a -> Bool) -> Set a -> Bool
allS p = all p . elemList
anyS p = any p . elemList

exactly ::
  Ord a => Int -> (a->Bool) -> Set a -> Bool
exactly n p =
  exactlyList n p . elemList
  where
  exactlyList 0 p xs     = not (any p xs)
  exactlyList n p []     = False
  exactlyList n p (x:xs) = exactlyList 
                             (if p x then n-1 else n) p xs

forAll, thereExists :: Ord a => Set a -> (a->Bool) -> Bool
forAll s p = allS p s
thereExists s p = anyS p s

forExactly :: Ord a => Int -> Set a -> (a->Bool) -> Bool
forExactly n s p = exactly n p s

mapS :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapS f = set . map f . elemList

-- more efficient variant when f is monotonic
mapMonoS :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapMonoS f = S . map f . elemList

unionMapS :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
unionMapS f = foldr (\/) emptyS . map f . elemList

minimalS :: Ord a => (Set a -> Bool) -> Set a -> Bool
minimalS p s = (p <| powerS s) == set [s]

regular :: Ord a => Int -> Set (Set a) -> Bool
regular d ss =
  -- every element occurs in exactly d sets
  forAll (unionS ss) $ \e ->
    forExactly d ss $ \s -> e <~ s
