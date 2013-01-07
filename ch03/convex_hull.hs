import Data.List

data Point = Point { x :: Float
                   , y :: Float
                   } deriving (Show, Eq)

data Direction = TurnLeft | TurnRight | Straight deriving (Show, Eq)

getDirection :: Point -> Point -> Point -> Direction
getDirection a b c
  | ccw > 0 = TurnLeft
  | ccw < 0 = TurnRight
  | otherwise = Straight
  where
    ccw = ((x b) - (x a)) * ((y c) - (y a))
          -
          ((y b) - (y a)) * ((x c) - (x a))

foldDirection :: [Point] -> [Direction]
foldDirection (x:y:z:xs) = getDirection x y z : foldDirection (y:z:xs)
foldDirection _ = []

convexHull :: [Point] -> [Point]
convexHull xs = convexHull' (lowest:(sortByAngle lowest $ filter filterLowest xs))
  where
    lowest = lowestPoint xs
    convexHull' (x:y:z:xs)
    -- FIXME this is WRONG
      | getDirection x y z == TurnLeft = x:convexHull' (y:z:xs)
      | otherwise                      = convexHull' (x:z:xs)
    convexHull' _ = []
    filterLowest = (\x -> x /= lowest)

lowestPoint :: [Point] -> Point
lowestPoint = minimumBy yOrdering
  where
    yOrdering a b
      | (y a) == (y b) = if ((x a) < (x b))
                            then LT
                            else
                              if (x a) > (x b)
                                 then GT
                                 else EQ
      | otherwise      = if ((y a) < (y b))
                            then LT
                            else
                              if (y a) > (y b)
                                 then GT
                                 else EQ

sortByAngle :: Point -> [Point] -> [Point]
sortByAngle lowest xs = sortBy (polarAngleOrdering lowest) xs

polarAngleOrdering :: Point -> Point -> Point -> Ordering
polarAngleOrdering lowest a b
  | angle lowest a < angle lowest b = LT
  | angle lowest a > angle lowest b = GT
  | otherwise                       = EQ
  where
    angle p1 p2
      | angle' < 0 = angle' + 180
      | otherwise  = angle'
        where
          angle' = atan (((y p2) - (y p1))/((x p2) - (x p1)))

a = [Point 1.2 3.2, Point (-10) (-11), Point 2.4 17.2, Point 9.1 2.0, Point (-1.2) 2, Point (-12) 12, Point (-1.2) (-8)]
