main :: IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364

isPalindrome :: Int -> Bool
isPalindrome x = (read $ reverse $ show x) == x

getPalindromes :: Int -> Int
getPalindromes x = minimum [d | d <- [2 .. x], isPalindrome d && mod x d == 0] + maximum [d | d <- [2 .. x], isPalindrome d && mod x d == 0]