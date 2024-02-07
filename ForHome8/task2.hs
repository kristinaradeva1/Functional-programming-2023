import Data.List

main :: IO()
main = do
    -- you may get slightly different results eg. [3, 4, 5] on test 1 <- not a problem
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] -- == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] -- == [2, 4, 6, 9]

type Nodes = Int
type Graph = (Nodes, Nodes, Nodes)

hasChildren :: [Graph] -> Nodes -> Bool
hasChildren g n = not $ null [ y | (x, y, z) <- g, x == n]

listLeaves :: [Graph] -> [Nodes]
listLeaves g = filter (not . hasChildren g) $ concatMap (\ (_, y, z) -> [y, z]) g
-- listLeaves g = concat [[z | (x, y, z) <- g, hasChildren g z == False], [y | (x, y, z) <- g, hasChildren g y == False]]