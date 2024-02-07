main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2



data Color = Red | Green | Blue
 deriving (Show, Eq)
data Tree = Empty | Node Color Tree Tree
 deriving (Show, Eq)

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

getLevel :: Tree -> Int -> [Color]
getLevel Empty _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

maxDepthFunc :: Tree -> Int
maxDepthFunc Empty = 0
maxDepthFunc (Node value left right) = maximum ([1 + maxDepthFunc left] ++ [1 + maxDepthFunc right])

maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode (Node value left right) = helper (Node value left right) maxDepth 
 where 
    maxDepth = maxDepthFunc (Node value left right)

    helper Empty maxDepth = maxDepth
    helper (Node value Empty Empty) maxDepth = maxDepth
    helper (Node value left right) 0 = 0
    helper (Node value left right) maxDepth
     | elem value (getLevel (Node value left right) maxDepth) && value == Blue = maxDepth
     | otherwise = helper right (maxDepth - 1)