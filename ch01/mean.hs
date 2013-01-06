mean :: (Num a) => [a] -> Float
mean xs = (sum xs) / (fromIntegral (length xs))
