main :: IO()
main = do
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6
    print $ sumDigitsIter (-13) -- error "n was negative"

-- дали може в начина с образците да се направи проверка за отрицателно число

sumDigitsIter :: Integer -> Integer
sumDigitsIter n
 | n < 0 = error "n was negative" 
 | otherwise = sumDigitsIter' n 0
 where
    sumDigitsIter' 0 sum = sum
    sumDigitsIter' num sum = mod num 10 + sumDigitsIter' (div num 10) (mod (div num 10) 10)