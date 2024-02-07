main :: IO()
main = do
    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0 == 2
    print $ countOccurrences 0 0 == 1

countOccurrences :: Int -> Int -> Int
countOccurrences 0 0 = 1
countOccurrences n digit = countOccurrences' n 0 
 where
    countOccurrences' 0 counter = counter
    countOccurrences' num counter 
     | mod num 10 == digit = countOccurrences' (div num 10) (counter + 1)
     | otherwise = countOccurrences' (div num 10) counter