main :: IO()
main = do
    print $ findJudge 2 [(1, 2)] == 2
    print $ findJudge 3 [(1, 3), (2, 3)] == 3
    print $ findJudge 3 [(1, 3), (2, 3), (3, 1)] == -1
    print $ findJudge 3 [(1, 2), (2, 3)] == -1
    print $ findJudge 4 [(1, 3), (1, 4), (2, 3), (2, 4), (4, 3)] == 3

findJudge :: Int -> [(Int, Int)] -> Int
findJudge 0 graph = -1
findJudge n graph 
 -- | isJudge 1 = 1
 | isJudge n = n
 | otherwise = findJudge (n - 1) graph
    where    
     isJudge :: Int -> Bool
     isJudge person = isLeaf person && everybodyTrusts person

     isLeaf :: Int -> Bool
     isLeaf person = null [ p | (p, c) <- graph, p == person]

     everybodyTrusts :: Int -> Bool
     everybodyTrusts person = (n - 1) == length [p | (p, c) <- graph, c == person] || n == length [p | (p, c) <- graph, c == person]
