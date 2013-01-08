-- file: ch04/ch04.exercises.hs
import Data.Char
import Data.List
import GHC.Base (maxInt)

maxIntAcc = maxInt `div` 10

asInt_fold :: String -> Int
asInt_fold [] = error ("Empty string")
asInt_fold "-" = error ("Just a minus sign")
asInt_fold ('-':xs) = negate $ asInt_fold xs
asInt_fold xs = foldl parseInt 0 xs
  where
  parseInt acc v
    | isDigit v = if acc > maxIntAcc
                     then error "Overflow"
                     else acc * 10 + digitToInt v
    | otherwise = error ("not a digit, bitch! " ++ "'" ++ [v] ++ "'")

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either [] = Left "Empty string"
asInt_either "-" = Left "Just a minus sign"
asInt_either ('-':xs) = fmap negate $ asInt_either xs
asInt_either xs = foldl parseInt (Right 0) xs
  where
  parseInt (Left err) _ = Left err
  parseInt (Right acc) v
    | isDigit v = if acc > maxIntAcc
                     then Left "Overflow"
                     else Right (acc * 10 + digitToInt v)
    | otherwise = Left ("not a digit, bitch! " ++ "'" ++ [v] ++ "'")

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | (f x) = x:takeWhile' f xs
  | otherwise = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold f = foldr (t f) []
  where
  t f x acc
    | (f x) = x:acc
    | otherwise = []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f (x:xs) = (x:ys):groupBy' f zs
                    where
                      (ys, zs) = span (f x) xs
groupBy' _ _ = []

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> if f x then not acc else acc) False

cycle' :: [a] -> [a]
cycle' xs = foldr (:) (cycle' xs) xs

words' :: String -> [String]
words' xs = delete "" $ nub $ foldr step [] xs
  where
    step x []
      | isSpace x = [[]]
      | otherwise = [[x]]
    step x (y:ys)
      | isSpace x = []:(y:ys)
      | otherwise = (x:y):ys

unlines' :: [String] -> String
unlines' xs = foldr step [] xs
  where
    step x acc = x ++ ('\n':acc)
