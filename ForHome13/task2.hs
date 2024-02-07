main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree :: (Num a) => BTree a
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

sumOfNodesGreaterOrEqualToANode :: (Num a, Ord a) => BTree a -> a -> a
sumOfNodesGreaterOrEqualToANode Nil _ = 0
sumOfNodesGreaterOrEqualToANode (Node value left right) k
 | value >= k = value + sumOfNodesGreaterOrEqualToANode left k + sumOfNodesGreaterOrEqualToANode right k
 | otherwise = sumOfNodesGreaterOrEqualToANode left k + sumOfNodesGreaterOrEqualToANode right k

convert :: (Num a, Ord a) => BTree a -> BTree a
convert Nil = Nil
convert (Node value left right) = (Node (sumOfNodesGreaterOrEqualToANode tree value) (convert left))  (convert right)