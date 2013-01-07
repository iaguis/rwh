import Data.List
import Test.QuickCheck
import Control.Applicative

data Point = Point { x :: Float
                   , y :: Float
                   } deriving (Show, Eq)

instance Arbitrary Point where
  arbitrary = Point <$> arbitrary <*> arbitrary

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

directions :: [Point] -> [Direction]
directions (x:y:z:xs) = getDirection x y z : directions (y:z:xs)
directions _ = []

convexHull :: [Point] -> [Point]
convexHull xs
  -- nothing to do if there's 3 or less points
  | length xs <= 3 = xs
  | otherwise =
    convexHull' [] (extend (sortByAngle lowest xs))
    where
      lowest = lowestPoint xs

      -- add first two points at the end to make it circular
      extend xs = xs ++ [xs !! 0] ++ [xs !! 1]

      convexHull' cHull (x:y:z:xs)
        -- turn left, keep x
        | getDirection x y z == TurnLeft = convexHull' (x:cHull) (y:z:xs)
        -- turn right, drop y and backtrack
        | otherwise                      = convexHull' (tail cHull) ((head cHull):x:z:xs)

      -- end of the list
      convexHull' cHull _ = reverse cHull -- reverse to make al turns ccw

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

-- sort by polar angle and put lowest point at the beginning
sortByAngle :: Point -> [Point] -> [Point]
sortByAngle lowest xs = lowest:sortBy (polarAngleOrdering lowest) (delete lowest xs)

polarAngleOrdering :: Point -> Point -> Point -> Ordering
polarAngleOrdering lowest a b
  | angle lowest a < angle lowest b = LT
  | angle lowest a > angle lowest b = GT
  | otherwise                       = EQ
  where
    angle p1 p2
      | angle' < 0 = angle' + pi
      | otherwise  = angle'
        where
          angle' = atan (((y p2) - (y p1))/((x p2) - (x p1)))

isConvex :: [Point] -> Bool
isConvex xs
  | length xs <= 3 = True
  | otherwise      = all (== TurnLeft) (directions xs)

a = [Point 1.2 3.2,
     Point (-10) (-11),
     Point (-4) 8,
     Point (-8) 10,
     Point 2.4 17.2,
     Point 9.1 2.0,
     Point (-1.2) 2,
     Point (-12) 12,
     Point (-1.2) (-8)]

prop_convexHull xs = isConvex $ convexHull xs
  where types = xs :: [Point]
