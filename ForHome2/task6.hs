main :: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 
    print $ sumOfDigits 242 == 8

sumOfDigits :: Int -> Int
sumOfDigits n = sumOfDigits' n 0 
 where
    sumOfDigits' n sum 
     | n > 0 = sumOfDigits' (div n 10) (mod n 10 + sum)
     | otherwise = sum

isInteresting :: Int -> Bool
isInteresting n = mod n (sumOfDigits n) == 0