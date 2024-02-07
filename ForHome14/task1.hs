main :: IO()
main = do
    print $ (getIndices [2, 7, 11, 15]) 9 == (0, 1)
    print $ (getIndices [3, 2, 4]) 6 == (1, 2)
    print $ (getIndices [3, 3]) 6 == (0, 1)


getIndices :: [Int] -> (Int -> (Int, Int))
getIndices ys = (\ x -> head [ (i1, i2) | (n1, i1) <- getIndexed, (n2, i2) <- getIndexed, i1 < i2 && n1 + n2 == x])
 where
    getIndexed = zip ys [0 ..]