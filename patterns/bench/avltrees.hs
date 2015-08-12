module Main where
import AVLTree
import Mutants


instance (Ord a, Enumerable a) => Enumerable (Tree a) where 
  enumerate = datatype [c1 fromList]

instance (Ord a, Parameter a) => Parameter (Tree a) where
  functions = lets "splitP" splitP -- FIXME: check if ok, adapted from heap


splitP :: Ord a => Tree a -> Maybe (a,Tree a)
splitP Empty = Nothing
splitP t = Just (x, remove x t)
  where x = v t
-- Trusting the root function above
-- Also using the actual findMin implementation (which won't be mutated)

-- * Tree Invariants:

ordered :: Ord a => Tree a -> Bool
ordered = orderedList . flatten
  where
    orderedList []       = True
    orderedList [_]      = True
    orderedList (x:y:xs) = x < y && orderedList (y:xs)

balanced :: Tree a -> Bool
balanced Empty              = True
balanced t@(Node _ lst _ rst) = abs (bf t) < 2 && balanced lst && balanced rst

underHeightLimit :: Tree a -> Bool
underHeightLimit t = n <= 2^h - 1
  where n = nElem t
        h = height t + 1

-- | Compares the height stored in the tree to an explicitly implemented version
correctHeight :: Tree a -> Bool
correctHeight t =  height t == explicitHeight t
  where
    explicitHeight Empty          = -1
    explicitHeight (Node _ lt _ gt) = max (height lt) (height gt) + 1


propertyMap :: (Ord a, Enumerable a)
            => Int
            -> Int
            -> ((a,Tree a) -> Tree a)
            -> ((a,Tree a) -> Tree a)
            -> ((a,Tree a) -> Maybe a)
            -> Bool
propertyMap n k insert'' remove'' find'' =
  [ runTests n $ \(x,t) -> ordered (insert' x t)             --  1
  , runTests n $ \(x,t) -> ordered (remove' x t)             --  2
  , runTests n $ \(x,t) -> balanced (insert' x t)            --  3
  , runTests n $ \(x,t) -> balanced (remove' x t)            --  4
  , runTests n $ \(x,t) -> underHeightLimit (insert' x t)    --  5
  , runTests n $ \(x,t) -> underHeightLimit (remove' x t)    --  6
  , runTests n $ \(x,t) -> correctHeight (insert' x t)       --  7
  , runTests n $ \(x,t) -> correctHeight (remove' x t)       --  8
  , runTests n $ \(x,t) -> find' x (insert' x t) == Just x   --  9
  , runTests n $ \(x,t) -> find' x (remove' x t) == Nothing  -- 10
  ] !! k
  where insert' = curry insert''
        remove' = curry remove''
        find'   = curry find''
        orderedList []       = True
        orderedList [_]      = True
        orderedList (x:y:xs) = x < y && orderedList (y:xs)

propertyMap' :: (Ord a, Enumerable a)
             => Int
             -> [((a,Tree a) -> Tree a
                 ,((a,Tree a) -> Tree a
                  ,(a,Tree a) -> Maybe a))
                -> Bool]
propertyMap' n = map uncurry3' $ map (propertyMap n) [0..9]

main :: IO ()
main = do runV (Just $ valid 4000) 5 (propertyMap' 4000) (uncurry insert :: (Int,Tree Int)->Tree Int,(uncurry remove,uncurry find))
          putStrLn "#### NOTE: ####"
          putStrLn "-- Rudy: The results above seem strange,"
          putStrLn "         double check if I didn't do anything wrong"

valid :: (Ord a, Enumerable a)
      => Int
      -> ((a,Tree a) -> Tree a
         ,((a,Tree a) -> Tree a
          ,(a,Tree a) -> Maybe a))
      -> ()
valid n (f,(g,h)) = v f `seq` v g `seq` v h where
  v a = runTests n ((`seq` True) . a) `seq` ()

uncurry3' :: (a -> b -> c -> d) -> (a,(b,c)) -> d
uncurry3' f = \(x,(y,z)) -> f x y z
