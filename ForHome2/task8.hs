main :: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5 
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6 
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134 

removeD :: Int -> Int -> Int
removeD digit num = removeD' digit num 0 1 
 where
    removeD' digit 0 result counter = result
    removeD' digit num result counter 
     | not (mod num 10 == digit) = removeD' digit (div num 10) (counter * mod num 10 + result) (counter * 10)
     | otherwise = removeD' digit (div num 10) result counter