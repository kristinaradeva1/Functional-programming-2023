main :: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True

-- isPresentRecNonPM :: Int -> [Int] -> Bool
-- isPresentRecNonPM _ [] = False
-- isPresentRecNonPM d (x:xs)
--  | d == x = True
--  | otherwise = isPresentRecNonPM d xs

isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM d xs
 | null xs = False
 | d == head xs = True
 | otherwise = isPresentRecNonPM d (tail xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM d (x:xs) = (x == d) || ((x /= d) && isPresentRecPM d xs)
      
isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc d xs = elem d xs