import Data.List

main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0
    
data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq, Read)

area :: (Num a, Floating a) => (Shape a) -> a
area (Circle r) = pi * r ^ 2
area (Rectangle x y) = x * y
area (Triangle x y z) = let p = (x + y + z) / 2 in sqrt $ p * (p - x) * (p - y) * (p - z)
area (Cylinder r h) = 2 * pi * r * h + 2 * pi * r^2

getAreas :: (Floating a) => [Shape a] -> [a]
getAreas = map area

-- maxArea :: (Ord a, Floating a) => [Shape a] -> Shape a
-- maxArea gs = head [s | s <- gs, area s == foldr1 max (getAreas gs)]

maxArea :: (Ord a, Floating a) => [Shape a] -> Shape a
maxArea gs = fst $ foldr1 (\ a1@(f1, s1) a2@(f2,s2) -> if s1 > s2 then a1 else a2) [(s, area s) | s <- gs]