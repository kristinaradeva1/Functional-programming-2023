main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

data NTree a = Nil | Node a [NTree a]
 deriving (Show)

t1 :: (Num a) => NTree a
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: (Num a) => NTree a
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]
 
getLevel :: NTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _) 0 = [value]
getLevel (Node value children) k = concatMap (\ tree -> getLevel tree (k - 1)) children 

getChildren :: (Eq a) => NTree a -> a -> [a]
getChildren (Node parent child) p = helper (Node parent child) p 0
 where
    helper Nil p counter = []
    helper (Node parent child) p counter
     | elem p (getLevel (Node parent child) counter) = getLevel (Node parent child) (counter + 1)
     | otherwise = helper (Node parent child) p (counter + 1)

isGraceful :: (Integral a) => NTree a -> Bool
isGraceful Nil = True
isGraceful (Node parent children) 
    | all (\ child -> even (parent - child)) (getChildren (Node parent children) parent) = all (\ child -> isGraceful child) children
    | otherwise = False