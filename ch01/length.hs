length' :: [a] -> Int
length' xs = len' xs 0
  where
    len' :: [a] -> Int -> Int
    len' [] n = n
    len' (_:xs) n = len' xs $! (n + 1)

main = putStrLn $ show $ length' [1..100000000000]
