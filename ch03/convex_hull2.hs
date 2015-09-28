data Point = Point {x :: Double, y :: Double}
    deriving (Show, Eq, Ord)

data Direction = TurnLeft | TurnRight | Straight
    deriving (Show, Eq)

turn :: Point -> Point -> Point -> Direction
turn a b c | prod > 0 = TurnLeft
           | prod > 0 = TurnRight
           | otherwise = Straight
     where prod = ((x(b) - x(a)) * (y(c) - y(a))) -
                  ((y(b) - y(a)) * (x(c) - x(a)))

direction :: [Point] -> [Direction]
direction (a:b:c:xs) = turn a b c : direction (b:c:xs)
direction _ = []

convexHull :: [Point] -> [Point]
convexHull xs = convexHull' [lowest] sorted
    where lowest = lowestY xs
          sorted = angleSort lowest xs

convexHull' :: [Point] -> [Point] -> [Point]
convexHull' _ [] = []
convexHull' [acc] (x:[]) = x : [acc]
convexHull' [acc] (x:xs)
    | turn lst acc x == Straight = [lst, acc]
        where lst = last xs
convexHull' (x:xs) (y:z:rsts) = case turn x y z of
    TurnRight -> convexHull' xs (x:z:rsts)
    Straight -> convexHull' (x:xs) (z:rsts)
    TurnLeft -> convexHull' (y:x:xs) (z:rsts)
convexHull' xs [z] = z:xs

angleSort :: Point -> [Point] -> [Point]
angleSort p = sortBy comp
    where comp x y = compare (angle p x) (angle p y)

angle :: Point -> Point -> Double
angle a b = atan2 (y(b) - y(a)) (x(b) - x(a)) * 180 / pi

lowestY :: [Point] -> Point
lowestY points = lowestY' (Point 0 100000) points
    where lowestY' l (p:ps)
            | y(p) < y(l) = lowestY' p ps
            | otherwise = lowestY' l ps
          lowestY' l [] = l

instance Arbitrary Point where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Point x y

-- QuickCheck
points :: Int -> Gen [Point]
points 0 = return []
points n = do { p <- arbitrary; ps <- points (n-1); return (p:ps) }

convex' = map(\(Point x y) -> x) . convexHull

prop_convex n = forAll (points n) (\xs -> sort (convexHull xs) == sort (convexHull (convexHull xs)))
