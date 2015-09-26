import Data.List

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
