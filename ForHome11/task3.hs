main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

t1 :: (Num a) => BTree (a, a) 
t1 = Node (3, 10) (Node (5, 8) (Node (6, 7) Nil Nil) (Node (4, 9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

t2 :: (Num a) => BTree (a, a)
t2 = Node (3, 10) (Node (5, 8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2, 12) Nil (Node (1, 15) Nil Nil))

getRoot :: BTree (a, a) -> (a, a) 
getRoot (Node value _ _) = value

isSubInterval :: (Ord a) => (a, a) -> (a, a) -> Bool
isSubInterval (x, y) (h, z) = x >= h && y <= z

ordered :: (Ord a) => BTree (a, a) -> Bool
ordered (Node _ Nil Nil) = True
ordered (Node _ Nil right) = (ordered right)
ordered (Node _ left Nil) = (ordered left)
ordered (Node value left right) 
 | isSubInterval value (getRoot left) = (ordered right)
 | isSubInterval value (getRoot right) = (ordered left) 
 | otherwise = False