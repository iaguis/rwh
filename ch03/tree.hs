data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = Node x Empty Empty
treeInsert x (Node a l r)
  | (x == a) = Node x l r
  | (x < a)  = Node a (treeInsert x l) r
  | (x > a)  = Node a l (treeInsert x r)

simpleTree = Node "p" (Node "l" Empty Empty) (Node "r" Empty Empty)
emptyTree = Node "p" Empty Empty

nums = [1, 4, 56, 3, 54, 7, 89, 5, 53, 24, 65, 87]
bigTree = foldr treeInsert Empty nums

