-- Imports {{{
import Data.List
import Data.Ord
-- }}}
-- Data types {{{
data Point = Point {
    pointXval :: Double,
    pointYval :: Double,
    name      :: String
} deriving (Show)

data Direction = StraightDir
               | LeftDir
               | RightDir
               deriving (Show, Eq)
-- }}}
-- Direction related methods {{{
pointSubtract :: Point -> Point -> Point
pointSubtract (Point p2x p2y _) (Point p1x p1y _) = Point (p2x - p1x) (p2y - p1y) "pointsToVector"

threePointDirection :: Point -> Point -> Point -> Direction
threePointDirection a b c
    | vectorDet >  0 = LeftDir
    | vectorDet == 0 = StraightDir
    | vectorDet <  0 = RightDir
    where
        Point v1x v1y _ = b `pointSubtract` a
        Point v2x v2y _ = c `pointSubtract` a
        vectorDet = v1x * v2y - v1y * v2x

pointsToDirections :: [Point] -> [Direction]
pointsToDirections [a,b,c] = [threePointDirection a b c]
pointsToDirections (a:b:c:xs) = [threePointDirection a b c] ++ pointsToDirections (b:c:xs)
-- }}}
-- Sorting {{{
sortByLowestYval :: [Point] -> [Point]
sortByLowestYval inList = sortBy compareLowestYval inList where
    compareLowestYval a b
        | pointYval a >  pointYval b = GT
        | pointYval a <  pointYval b = LT
        | pointYval a == pointYval b = compare (pointXval a) (pointXval b)

sortByCoTan :: [Point] -> [Point]
sortByCoTan inList = pivotPoint : sortBy compareCoTan remainingPoints where
    pointYvalSortedList = sortByLowestYval inList
    pivotPoint          = head pointYvalSortedList
    remainingPoints     = tail pointYvalSortedList
    compareCoTan a b
        | a_CoTan   > b_CoTan = GT
        | a_CoTan   < b_CoTan = LT
        | aY        > bY      = GT
        | aY        < bY      = LT
        | otherwise           = EQ
        where
            pivotX  = pointXval pivotPoint
            pivotY  = pointYval pivotPoint
            aX      = pointXval a
            aY      = pointYval a
            bX      = pointXval b
            bY      = pointYval b
            a_CoTan = (aX - pivotX) / (aY - pivotY)
            b_CoTan = (bX - pivotX) / (bY - pivotY)
            -- TODO: use a calcCoTan method instead of two variables
-- }}}
-- Scan algorithm {{{
grahamScan :: [Point] -> [Point]
grahamScan [a,b,c] = [a,b,c]
grahamScan inList = checkTurns toScan [initialPoint] where
    initialPoint = head sortedList
    sortedList   = sortByCoTan inList
    toScan       = tail sortedList ++ take 2 sortedList

    checkTurns (a:b:c:xs) output
        | threePointDirection a b c == LeftDir     = checkTurns (last output:a:c:xs) (init output)
        | otherwise                                = checkTurns (b:c:xs) (output ++ [a])
    checkTurns _ output = output
-- }}}
-- Debugging {{{
namePointRepresentation :: [Point] -> String
namePointRepresentation inList = concat $ intersperse " - " (pointNames inList) where
    pointNames [] = []
    pointNames (x:xs) = name x : pointNames xs
-- }}}
