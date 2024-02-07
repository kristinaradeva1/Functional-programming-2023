main :: IO()
main = do
    print $ (switchsum (\x -> x + 1) (\x -> x * 2) 1) 2 -- == 3
    print $ (switchsum (\x -> x + 1) (\x -> x * 2) 2) 2 -- == 9
    print $ (switchsum (\x -> x + 1) (\x -> x * 2) 3) 2 -- == 16
    print $ (switchsum (\x -> x + 1) (\x -> x * 2) 4) 2 -- == 30

switchsum :: (Num a, Eq a) => (a -> a) -> (a -> a) -> a -> (a -> a)
switchsum f g 1 = f 
switchsum f g n = (\ x -> f x + (switchsum g f (n - 1) (f x)))