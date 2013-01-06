data Point = Point { x :: Float
                   , y :: Float
                   } deriving (Show)
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

a = [Point 1.2 3.2, Point 2.4 32, Point 9.1 2.0, Point (-1.2) (-8), Point (-12) 12]
