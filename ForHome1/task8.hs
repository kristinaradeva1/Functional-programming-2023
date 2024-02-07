main :: IO()
main = do
    print $ snail 3 2 1 == 2
    print $ snail 10 3 1 == 5
    print $ snail 10 3 2 == 8
    print $ snail 100 20 5 == 7
    print $ snail 5 10 3 == 1

snail :: Int -> Int -> Int -> Int
snail column day night = snail' 0 0 0
 where 
    snail' curHeight counter days
     | day >= column = 1
     | column <= curHeight && days < 2 = counter
     | column <= curHeight && days >= 2 = days 
     | even counter = snail' (curHeight + day) (counter + 1) (days + 1)
     | otherwise = snail' (curHeight - night) (counter + 1) days