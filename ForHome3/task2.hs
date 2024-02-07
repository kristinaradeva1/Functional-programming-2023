main :: IO()
main = do
    print $ sortN 1714 == 7411
    print $ sortN 123450 == 543210
    print $ sortN 123405 == 543210
    print $ sortN 123045 == 543210
    print $ sortN 120345 == 543210
    print $ sortN 102345 == 543210
    print $ sortN 8910 == 9810
    print $ sortN 321 == 321
    print $ sortN 29210 == 92210
    print $ sortN 1230 == 3210
    print $ sortN 55345 == 55543
    print $ sortN 14752 == 75421
    print $ sortN 329450 == 954320
    print $ sortN 9125 == 9521

containsDigit :: Int -> Int -> Bool
containsDigit 0 digit = False
containsDigit n digit 
 | mod n 10 == digit = True
 | otherwise = containsDigit (div n 10) digit

removeFirstOccurrence :: Int -> Int -> Int
removeFirstOccurrence num digit = removeFirstOccurrence' num digit 0 1 0
 where
    removeFirstOccurrence' 0 digit ocur count result = result
    removeFirstOccurrence' num digit occur count result
     | (mod num 10 == digit && occur > 0 ) = removeFirstOccurrence' (div num 10) digit (occur + 1) (count * 10) (mod num 10 * count + result)
     | mod num 10 == digit = removeFirstOccurrence' (div num 10) digit (occur + 1) count result
     | otherwise = removeFirstOccurrence' (div num 10) digit occur (count * 10) (mod num 10 * count + result)

findMax :: Int -> Int
findMax n = helper (div n 10) (mod n 10)
 where
    helper :: Int -> Int -> Int
    helper 0 result = result
    helper leftover result
     | mod leftover 10 > result = helper (div leftover 10) (mod leftover 10)
     | otherwise = helper (div leftover 10) result

sortN :: Int -> Int
sortN n = sortN' n n max newN 10 0
 where
    max = findMax n
    newN = removeFirstOccurrence n max 
    sortN' n saveN max newN count result
     | n == 0 && containsDigit saveN 0 = result * 10
     | n > 0 = sortN' newN saveN (findMax newN) (removeFirstOccurrence newN (findMax newN)) count (result * count + max)
     | otherwise = result    