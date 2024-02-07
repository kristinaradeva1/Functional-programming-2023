main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2

data Color = Red | Green | Blue
data Tree = Empty | Node Color Tree Tree

minDepthGreenNode :: Tree -> Int
minDepthGreenNode Empty = 0
minDepthGreenNode (Node value left right) = minimum ([1 + minDepthGreenNode right] ++ [1 + minDepthGreenNode left])
 
colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)