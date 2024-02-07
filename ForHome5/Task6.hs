main :: IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8

pairCompose :: (Num a) => [(a -> a)] -> (a -> a)
pairCompose [] = (\ x -> x)
pairCompose [f] = (\ x -> f x)
pairCompose (z:y:ys) = (\ x -> z $ y x + pairCompose ys x)