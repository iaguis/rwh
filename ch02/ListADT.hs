data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
            deriving (Show)
