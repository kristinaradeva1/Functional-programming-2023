main :: IO()
main = do
    print $ isArithmentic [3] == True
    print $ isArithmentic [3, 5] == True
    print $ isArithmentic [1, 2, 3, 4, 5] == True
    print $ isArithmentic [3, 5, 7, 9, 11] == True
    print $ isArithmentic [3, 5, 8, 9, 11] == False
    print $ isArithmentic [3, 5, 9, 9, 11] == False

-- isArithmentic :: [Int] -> Bool
-- isArithmentic [z] = True
-- isArithmentic xs = (xs !! 1 == (xs !! 0 + k))  && isArithmentic (tail xs)
--  where
--     k = (xs !! 1 - xs !! 0)

isArithmentic :: [Int] -> Bool
isArithmentic xs = length xs < 3 || (xs !! 1 - xs !! 0 == xs !! 2 - xs !! 1) && isArithmentic (tail xs)
