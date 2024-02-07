main :: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98


findSum :: Int -> Int -> Int -> Int
findSum a b n = findSum' (n - 3) a
 where
    findSum' counter sum
     | counter == (-1)  = findSum' (counter - 1) (sum + 2^(n + counter - 1) * b + sum + 2^(n + counter - 1) * b + 2^(n + counter) * b + sum)
     | counter >= 0 = findSum' (counter - 1) (sum + 2^counter * b)
     | otherwise = sum