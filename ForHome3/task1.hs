main :: IO()
main = do 
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence num digit = removeFirstOccurrence' num digit 0 1 0
 where
    removeFirstOccurrence' 0 digit ocur count result = result
    removeFirstOccurrence' num digit occur count result
     | mod num 10 == digit && occur > 0 = removeFirstOccurrence' (div num 10) digit (occur + 1) (count * 10) (mod num 10 * count + result)
     | mod num 10 == digit = removeFirstOccurrence' (div num 10) digit (occur + 1) count result
     | otherwise = removeFirstOccurrence' (div num 10) digit occur (count * 10) (mod num 10 * count + result)