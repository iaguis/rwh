splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f l = let (a, l'') = span f l
                    in case dropWhile f l of
                     -- last group
                     [] -> [a]
                     l' -> a : splitWith f (tail l'')
