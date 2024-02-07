main :: IO()
main = do
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsIter :: Int -> Int
countDigitsIter n 
     | n < 0 = error "n was negative"
     | otherwise = countDigitsIter' n 0
 where 
    countDigitsIter' 0 sum = sum
    countDigitsIter' num sum = countDigitsIter' (div num 10) (sum + 1)
    
countDigitsRec :: Int -> Int
countDigitsRec 0 = 0
countDigitsRec n 
 | n < 0 = error "n was negative"
 | otherwise = 1 + countDigitsRec (div n 10)