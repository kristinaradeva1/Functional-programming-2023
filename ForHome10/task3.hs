main::IO()
main = do
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
    print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)

    print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)

data Point a = TwoD a a | ThreeD a a a
 deriving (Show, Eq, Read, Ord) 

findDistance :: (Floating a) => Point a -> Point a -> a 
findDistance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = (sqrt((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2))
findDistance (TwoD x1 y1) (TwoD x2 y2) = (sqrt((x2 - x1)^2 + (y2 - y1)^2))

getAllDistances :: (Floating a) => [Point a] -> [(a, Point a, Point a)]
getAllDistances (x:xs) = [ (findDistance s x, x, s) | s <- xs]

getClosestDistance :: (Num a, Floating a, Ord a) => [Point a] -> (a, Point a, Point a)
getClosestDistance [] = error "None"
getClosestDistance xs = foldr1 (\ p1@(dist, point1, point2) p2@(dist1, point3, point4) -> if dist < dist1 then p1 else p2) (getAllDistances xs)