main :: IO()
main = do
    print $ canCarry 5 15 3 == "Yes"
    print $ canCarry 1 5 4 == "Yes"
    print $ canCarry 13 25 2 == "No"
    print $ canCarry 24 104.44 21.12 == "No"
    print $ canCarry 51 34.75 19.852 == "No"
    print $ canCarry 42 95.11 0.51 == "Yes"

    print $ canCarry (-13) 25 2 -- error: The number of products was negative
    print $ canCarry 13 (-25) 2 -- error: John's hosting capacity was negative
    print $ canCarry 13 25 (-2) -- error: The weight of a product was negative

-- c : number of product
-- k : kilograms he can carry
-- w : kilograms of an item from the supermarket

canCarry :: Int -> Double -> Double -> String
canCarry c k w = canCarry' doubleC k w 
 where
     doubleC = fromIntegral c
     | k < 0 = error "John's hosting capacity was negative"
     | doubleC < 0 = error "The number of products was negative"
     | w < 0 = error "The weight of a product was negative"
     | doubleC * w > k = "No"
     | otherwise = "Yes"