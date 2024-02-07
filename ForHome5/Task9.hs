main :: IO()
main = do
    print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
    print $ dotProduct (5, 2, 159) (0, -1, -2) == (-320)

    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053

type Vector a = (Num a) => (a, a, a)

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct (a1, a2, a3) (b1, b2, b3) = a1 * b1 + a2 * b2 + a3 * b3

crossProduct :: (Num a) => Vector a -> Vector a -> Vector a
crossProduct (a1, a2, a3) (b1, b2, b3) = (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

magnitude :: (Floating a, Num a) => Vector a -> a
magnitude (a1, a2, a3) = sqrt (a1^2 + a2^2 + a3^2)