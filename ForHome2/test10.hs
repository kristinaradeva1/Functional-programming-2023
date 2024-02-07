main :: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

rev :: Int -> Int
rev n = rev' n 0
 where
    rev' 0 result = result
    rev' n result = rev' (div n 10) ((mod n 10) + 10 * result)

isPalindrome :: Int -> Bool
isPalindrome n = (rev n == n) 

countPalindromes :: Int -> Int -> Int
countPalindromes num1 num2 = countPalindromes' (min num1 num2) (max num1 num2) 0
 where
    countPalindromes' realStart realEnd counter
     | realEnd == realStart = counter
     | isPalindrome realEnd = countPalindromes' realStart (realEnd - 1) (counter + 1)
     | otherwise = countPalindromes' realStart (realEnd - 1) counter