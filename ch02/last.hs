last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last xs

lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [x] = error "one element list"
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs
