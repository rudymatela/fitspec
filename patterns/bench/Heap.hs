module Main where
import Mutants
import Prelude hiding (null)
import qualified Data.List as L

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
infixr 0 ==>


type A = [Bool]
propertyMap :: Int
            -> Int
            -> ((A,Heap A) -> Heap A
               ,((Heap A -> Maybe (Heap A))
               ,(Heap A,Heap A) -> Heap A))
            -> Bool
propertyMap n k (insert'', (deleteMin', merge'')) =
  [ runTests n $ \(x,y,h) ->      insert' x (insert' y h) == insert' y (insert' x h) --  1
  , runTests n $ \(h,x) ->             null (insert' x h) == False                   --  2
  , runTests n $ \(x,h) ->          L.insert x (toList h) == toList (insert' x h)    --  3

  ,                                                      True                        --  4

  , runTests n $ \(h,h1) ->                   merge' h h1 == merge' h1 h             --  5
  , runTests n $ \h ->                       merge' h Nil == h                       --  6
  , runTests n $ \(h,h1,h2) ->    merge' h (merge' h1 h2) == merge' h1 (merge' h h2) --  7
  , runTests n $ \h ->              findMinP (merge' h h) == findMinP h              --  8
  , runTests n $ \h ->                  null (merge' h h) == null h                  --  9
  , runTests n $ \(h,h1) ->           (null h && null h1) == null (merge' h h1)      -- 10

  , runTests n $ \(h,h1,x) ->     merge' h (insert' x h1) == insert' x (merge' h h1) -- 11
  , runTests n $ \h ->     fmap (merge' h) (deleteMin' h) == deleteMin' (merge' h h) -- 12
  , runTests n $ \x ->         deleteMin' (insert' x Nil) == Just Nil                -- 13
  ,                                        deleteMin' Nil == Nothing                 -- 14
  ] !! (k-1)
  where merge' = curry merge''
        insert' = curry insert''

propertyMap' :: Int
            -> [((A,Heap A) -> Heap A
               ,((Heap A -> Maybe (Heap A))
               ,(Heap A,Heap A) -> Heap A))
            -> Bool]
propertyMap' n = map (propertyMap n) [1..14]


main :: IO () 
main = runM 11 (propertyMap' 2000) (uncurry insert,(deleteMinP,uncurry merge))

data Heap a = Nil | Branch Int a (Heap a) (Heap a) deriving Show

instance (Ord a, Enumerable a) => Enumerable (Heap a) where 
  enumerate = datatype [c1 fromList] -- Relying on the tested code a bit...
                      -- [c0 Nil, c4 Branch]

instance Parameter a => Parameter (Heap a) where
  functions = signature go where
    go (n,b) = [p0 "Nil" null n, p4 "Branch" extB b]
    extB (Branch a b c d) = Just (a,b,c,d)
    extB _                = Nothing

-- Maybe not a good instance for Ints, but works as a placeholder
instance Parameter Int where
  functions = signature go where
    go (ex,a,b) = [p0 (show (ex :: Int)) (== ex) a, p0 "_" (/= ex) b]


instance Ord a => Eq (Heap a) where
  h1 == h2 = toList h1 == toList h2

toList :: Ord a => Heap a -> [a]
toList Nil = []
toList h   = findMin h : toList (deleteMin h)

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert Nil

null :: Heap a -> Bool
null Nil = True
null _ = False

findMin :: Heap a -> a
findMin (Branch _ x _ _) = x

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge h (branch x Nil Nil)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Branch _ _ l r) = merge l r

branch :: Ord a => a -> Heap a -> Heap a -> Heap a
branch x l r | npl l <= npl r = Branch (npl l + 1) x l r
             | otherwise = Branch (npl r + 1) x r l

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge h1@(Branch _ x1 l1 r1) h2@(Branch _ x2 l2 r2)
 | x1 <= x2 = branch x1 (merge l1 h2) r1
 | otherwise = merge h2 h1

npl :: Heap a -> Int
npl Nil = 0
npl (Branch n _ _ _) = n

deleteMinP h | null h  = Nothing 
deleteMinP h           = Just $ deleteMin h
        
findMinP h | null h = Nothing
findMinP h          = Just $ findMin h

