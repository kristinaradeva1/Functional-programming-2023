main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree a = Nil | Node a (BTree a) (BTree a)

numberBTree :: (Num a) => BTree a 
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

levelSum :: (Num a) => BTree a -> Int -> a
levelSum Nil _ = 0
levelSum (Node value left right) 0 = value
levelSum (Node value left right) k = sum $ getLevel (Node value left right) k 

cone :: (Num a, Ord a) => BTree a -> Bool
cone (Node value left right) = helper (Node value left right) 0 
 where
    helper (Node value Nil Nil) _ = True
    helper (Node value left right) counter 
     | (levelSum (Node value left right) counter) < (levelSum (Node value left right) (counter + 1)) = helper (Node value left right) (counter + 1)
     | helper (Node value Nil Nil) counter = True 
     | otherwise = False