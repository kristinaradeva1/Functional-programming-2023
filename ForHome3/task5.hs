main :: IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

p :: Int -> Int
p 1 = 1
p n = p' 2 0 0 
 where
    p' counter remover result
     | counter == 2 = p' (counter + 1) 3 (((counter * 5 - 5) - remover) + result)
     | counter <= n = p' (counter + 1) (remover + 2) (((counter * 5 - 5) - remover) + result)
     | otherwise = result