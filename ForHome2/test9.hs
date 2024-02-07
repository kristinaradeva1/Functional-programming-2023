import Data.List

main :: IO()
main = do
    print $ everyOther 12 == 1
    print $ everyOther 852369 == 628
    print $ everyOther 1714 == 11
    print $ everyOther 12345 == 42
    print $ everyOther 891 == 9
    print $ everyOther 123 == 2
    print $ everyOther 2121 == 22
    print $ everyOther 4736778 == 767
    print $ everyOther 448575 == 784
    print $ everyOther 4214 == 14
-- 852369 == 628
rev :: Int -> Int
rev n = rev' n 0
 where
    rev' 0 result = result
    rev' n result = rev' (div n 10) (mod n 10 + 10 * result)

everyOther :: Int -> Int
everyOther n = everyOther' n n 0 1 1 
 where
    everyOther' n 0 result counter counter2 = rev result
    everyOther' n leftover result counter counter2
     | (mod counter 2 == 0) = everyOther' n (div leftover 10) (mod leftover 10 * counter2 + result) (counter + 1) (counter2 * 10)
     | otherwise = everyOther' n (div leftover 10) result (counter + 1) counter2