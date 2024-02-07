main :: IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 == [[2,3],[2,4]]

type Node = Int
type Graph = [(Node, [Node])]
type Path = [Node]

getChildren :: Node -> Graph -> [Node]
getChildren n g = concat [ y | (x, y) <- g, x == n] 

longestPath :: Graph -> Node -> [Path]
longestPath g n = [ [x, y, z] | (x, [y, z]) <- g, x == n] ++ [ [x, y, head $ reverse (getChildren y g)] | (x, [y, z]) <- g, x == n]

simplePaths :: Graph -> Int -> Node -> [Path]
simplePaths g 0 n = [[n]]
simplePaths g 1 n = [ [x, head $ getChildren x g] | (x, [y, z]) <- g, x == n] ++ [ [x, head $ reverse (getChildren x g)] | (x, [y, z]) <- g, x == n]
simplePaths g k n 
 | k > (length $ longestPath g n) = error "there is no path with such length"
 | otherwise = (longestPath g n) 