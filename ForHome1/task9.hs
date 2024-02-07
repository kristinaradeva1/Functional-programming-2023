main :: IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789

rev :: Int -> Int
rev n = rev' n 0 
 where
    rev' 0 result = result
    rev' n result = rev' (div n 10) (mod n 10 + result * 10)