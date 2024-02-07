main :: IO()
main = do
    print $ (myPoly [2.7, 3.0 ..]) 2.2 3 == -0.4399999999999998

myPoly :: (Floating a) => [a] -> (a -> Int -> a)
myPoly zs = (\ x y -> product (zipWith (\ x z -> x - z) (replicate y x) (take y zs)))