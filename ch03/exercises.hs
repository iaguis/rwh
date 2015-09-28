import Data.List
import Test.QuickCheck
import Control.Monad

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

listMean :: (Fractional a) => [a] -> a
listMean xs = sum xs / l
    where l = fromIntegral $ length' xs

makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ (reverse xs)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

sortLength :: [[a]] -> [[a]]
sortLength = sortBy comp
    where comp xs ys = compare (length' xs) (length' ys)

intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = x
intersperse' sep (x:xs) = x ++ [sep] ++ intersperse' sep xs

data Tree a = Node a (Tree a) (Tree a)
    | Empty
      deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ lt rt) = 1 + max (treeHeight lt) (treeHeight rt)
