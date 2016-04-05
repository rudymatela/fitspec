module AVLTree
  (
  -- | * External API
    empty
  , insert
  , remove
  , find
  , preOrder
  , inOrder
  , postOrder
  , flatten
  , height
  , nElem
  , isEmpty
  , fromList

  , leaf
  , (//)
  , (\\)
  , (-/)
  , (\-)

  -- | * Internal API
  , Tree(..)
  , node
  , bf
  , v
  , l
  , r
  , rotatell
  , rotaterr
  , rotatelr
  , rotaterl
  , balance
  , same
  , removeRoot
  , removeGreatest
  )
where

-- | Tree definition
data Tree a = Empty |
              Node Int (Tree a) a (Tree a)

-- | Smart node constructor that infers height from given subtrees
node :: Tree a -> a -> Tree a -> Tree a
node lst x rst = Node (max (height lst) (height rst) + 1) lst x rst

-- | Smart node constructor for leafs
leaf :: a -> Tree a
leaf x = node empty x empty

empty :: Tree a
empty = Empty

-- | Left infix tree constructor
(//) :: Tree a -> a -> (Tree a -> Tree a)
(//) = node
infix 6 //

-- | Left infix tree constructor (leaf value)
(-/) :: a -> a -> (Tree a -> Tree a)
x -/ y = node (leaf x) y
infix 6 -/

-- | Right infix tree constructor
(\\) :: (Tree a -> Tree a) -> Tree a -> Tree a
(\\) = ($)
infix 5 \\

-- | Right infix tree constructor (leaf value)
(\-) :: (Tree a -> Tree a) -> a -> Tree a
ctx \- x = ctx (leaf x)
infix 5 \-


-- | Shows tree in format (1-/2\-3)//4\\(empty//5\-7)
instance (Show a) => Show (Tree a) where
  showsPrec _ Empty                = showString "empty"
  showsPrec d (Node _ Empty x Empty) = showParen (d>9) $ showString "leaf " . showsPrec 10 x
  showsPrec d (Node _ lst   x   rst) = showParen (d>4) $ left . showsPrec 7 x . right
    where left  | isLeaf lst  = showsPrec 7 (v lst) . showString "-/"
                | otherwise   = showsPrec 6 lst . showString "//"
          right | isLeaf rst  = showString "\\-" . showsPrec 7 (v rst)
                | otherwise   = showString "\\\\" . showsPrec 6 rst

-- | Two trees are equal if they hold the same elements.  To check for equality also on the structure of the tree, use "same"
instance (Eq a) => Eq (Tree a) where
  t == u  =  flatten t == flatten u

instance (Ord a) => Ord (Tree a) where
  t `compare` u  =  flatten t `compare` flatten u

-- | The function should map values keeping ordering, otherwise you'll get a
-- problematic AVL.  The resulting AVL can only be manipulated by 'insert' and
-- 'delete' if it follows the 'Invariants.ordered'.
instance Functor Tree where
  fmap _ Empty              = Empty
  fmap f (Node h lst x rst) = Node h (fmap f lst) (f x) (fmap f rst)


-- | Two trees are **same** if their *values* and *structure* is the same.
-- Every **same** pair of 'Tree's is '==', not every '==' pair of 'Tree's is
-- **same**
infix 4 `same`
same :: Eq a => Tree a -> Tree a -> Bool
Empty                `same` Empty = True
(Node _ tlst x trst) `same` (Node _ ulst y urst) =  x == y && tlst `same` ulst && trst `same` urst
_                    `same` _ = False


insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = node Empty x Empty
insert x t@(Node _ lt y gt) = balance u
  where u = case x `compare` y of
              EQ -> t
              LT -> node (insert x lt) y gt
              GT -> node lt y (insert x gt)


remove :: Ord a => a -> Tree a -> Tree a
remove _ Empty            = Empty -- no-op
remove x t@(Node _ lst y rst) = balance $
  case x `compare` y of
    EQ -> removeRoot t
    LT -> remove x lst
    GT -> remove x rst


removeRoot :: Tree a -> Tree a
removeRoot Empty                = Empty
removeRoot (Node _ Empty _ Empty) = Empty
removeRoot (Node _ lst   _ Empty) = lst
removeRoot (Node _ Empty _ rst)   = rst
removeRoot (Node _ lst   _ rst)   = balance (node nlst y rst)
  where
    (y, nlst) = removeGreatest lst


removeGreatest :: Tree a -> (a, Tree a)
removeGreatest Empty                  = errorEmptyTree "removeGreatest"
removeGreatest (Node _ lst   x Empty) = (x, lst)
removeGreatest (Node _ lst   x rst)   = (y, balance (node lst x nrst))
  where
    (y, nrst) = removeGreatest rst


find :: Ord a => a -> Tree a -> Maybe a
find _ Empty = Nothing
find x (Node _ lt y gt) =
  case x `compare` y of
    EQ -> Just y
    LT -> find x lt
    GT -> find x gt


preOrder  :: Tree a -> [a]
preOrder  Empty             =  []
preOrder  (Node _ lst x rst)  =  [x] ++ preOrder lst ++ preOrder rst

inOrder   :: Tree a -> [a]
inOrder   Empty             =  []
inOrder   (Node _ lst x rst)  =  inOrder lst ++ [x] ++ inOrder rst

postOrder :: Tree a -> [a]
postOrder Empty             =  []
postOrder (Node _ lst x rst)  =  postOrder lst ++ postOrder rst ++ [x]

-- | Alias for inOrder
flatten :: Tree a -> [a]
flatten = inOrder

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty


-- | Height of a Tree
height :: Tree a -> Int
height Empty          = -1
height (Node h _ _ _) = h

-- | Number of values stored in the tree.  Note: this is slow, as it actually
-- evaluates the whole "spine" of the tree.
nElem :: Tree a -> Int
nElem Empty  = 0
nElem (Node _ lt _ gt) = nElem lt + nElem gt + 1

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

isLeaf :: Tree a -> Bool
isLeaf (Node _ Empty _ Empty) = True
isLeaf _                      = False

-- | Balancing factor of a Tree
bf :: Tree a -> Int
bf Empty          = 0
bf (Node _ lt _ gt) = height lt - height gt

-- | Value of a node (root)
v :: Tree a -> a
v (Node _ _ x _) = x
v Empty = errorEmptyTree "v"

-- | Left subtree
l :: Tree a -> Tree a
l (Node _ lst _ _) = lst
l Empty = errorEmptyTree "l"

-- | Right subtree
r :: Tree a -> Tree a
r (Node _ _ _ rst) = rst
r Empty = errorEmptyTree "r"

rotatell :: Tree a -> Tree a
rotatell (Node _ (Node _ llst y lrst) x rst) = node llst y (node lrst x rst)
rotatell _ = errorEmptySubtree "rotatell"

rotaterr :: Tree a -> Tree a
rotaterr (Node _ lst x (Node _ rlst y rrst)) = node (node lst x rlst) y rrst
rotaterr _ = errorEmptySubtree "rotaterr"

rotatelr :: Tree a -> Tree a
rotatelr (Node _ lst x rst) = rotatell (node (rotaterr lst) x rst)
rotatelr _ = errorEmptySubtree "rotatelr"

rotaterl :: Tree a -> Tree a
rotaterl (Node _ lst x rst) = rotaterr (node lst x (rotatell rst))
rotaterl _ = errorEmptySubtree "rotaterl"

balance :: Tree a -> Tree a
balance t | bf t > 1   =  if bf (l t) == (-1)
                            then rotatelr t
                            else rotatell t
          | bf t < -1  =  if bf (r t) == 1
                            then rotaterl t
                            else rotaterr t
          | otherwise  =  t


errorEmptyTree :: String -> a
errorEmptyTree    fun = err fun "empty tree (trying to balance non-AVL tree?)"

errorEmptySubtree :: String -> a
errorEmptySubtree fun = err fun "empty subtree (trying to balance non-AVL tree?)"

err :: String -> String -> a
err fun msg = error ("AVLTree.Internals." ++ fun ++ ": " ++ msg)

