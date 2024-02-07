main :: IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910


growingPlant :: Int -> Int -> Int -> Int
growingPlant upSpeed downSpeed desiredHeight = growingPlant' 0 0 0 
 where
    growingPlant' curHeight counter days
     | upSpeed >= desiredHeight = 1
     | desiredHeight == curHeight && days < 2 = counter
     | desiredHeight == curHeight && days >= 2 = days 
     | even counter = growingPlant' (curHeight + upSpeed) (counter + 1) (days + 1)
     | otherwise = growingPlant' (curHeight - downSpeed) (counter + 1) days