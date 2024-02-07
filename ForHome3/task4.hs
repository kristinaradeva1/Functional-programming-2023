main :: IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990

sumOfDigits :: Int -> Int
sumOfDigits n = sumOfDigits' n 0
 where 
    sumOfDigits' n sum
     | n > 0 = sumOfDigits' (div n 10) (mod n 10 + sum)
     | otherwise = sum

-- линейно-итеративен процес

-- sumDivisibleNumbers :: Int -> Int -> Int -> Int
-- sumDivisibleNumbers start finish k = sumDivisibleNumbers' (min start finish) (max start finish) 0
--  where
--     sumDivisibleNumbers' start finish sum
--      | finish == start = sum
--      | mod (sumOfDigits finish) k == 0 = sumDivisibleNumbers' start (finish - 1) (sum + finish)
--      | otherwise = sumDivisibleNumbers' start (finish - 1) sum

-- линейно-рекурсивен процес

sumDivisibleNumbers :: Int -> Int -> Int -> Int
sumDivisibleNumbers start finish k = sumDivisibleNumbers' (min start finish) (max start finish)
 where
    sumDivisibleNumbers' start1 finish1
     | finish1 == start1 = 0
     | mod (sumOfDigits finish1) k == 0 = finish1 + sumDivisibleNumbers' start1 (finish1 - 1)
     | otherwise = sumDivisibleNumbers' start1 (finish1 - 1)