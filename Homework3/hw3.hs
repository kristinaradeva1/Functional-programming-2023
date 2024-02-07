main :: IO()
main = do
    print $ length [T 1 [T 3 [T 5 [], T 6[]]]]
    print $ isBranch (T 1 [])
    print $ prune t -- == T 1 [T 2 [T 3 []], T 4 [T 5 []], T 7 [T 8 [], T 9 [T 10 []]]]

data NTree a = T a [(NTree a)]
 deriving (Eq, Show, Foldable)

t :: (Num a) => NTree a
t = T 1 [T 2 [T 3 []], T 4 [T 5 [T 6 []]], T 7 [T 8 [], T 9 [T 10 [T 11 []]]]] 

--пръчка -> вс връх има най-много 1 дете

depth :: NTree a -> Int
depth (T parent []) = 1 
depth (T parent children) = 1 + (sum $ map depth children) 

-- isBranch :: NTree a -> Bool   --да има само 1 дете 
-- isBranch (T parent []) = True
-- isBranch (T parent children) = length children == 1 && (all (\ child -> (isBranch child)) children) 
-- -- второ условие && (depth (T parent children) > 2)

isBranch :: NTree a -> Bool   --да имат само 1 дете 
isBranch (T parent children) = (length children == 1 || length children == 0) && (all (\ child -> (isBranch child)) children) 
-- това е условието за branch, но има и друго условие за поне 2 нода

-- prune :: NTree a -> NTree a 
-- prune (T parent [(T children [])]) = (T parent [])
-- prune (T parent cs@[(T children grandChildren)]) 

prune :: NTree a -> NTree a
prune (T parent cs@[(T children grandChildren)]) = (T parent (map (\ childSubtree -> if isBranch childSubtree then (takeWhile (\ childSubtree -> depth childSubtree /= 2) cs) else (map (\ grandChildSubtree -> prune grandChildSubtree) grandChildren)) cs))

-- (T parent (map (\ childSubtree -> if isBranch childSubtree then (dropWhile (\ childSubtree -> depth childSubtree == 2) cs) else map (\ child -> prune child) cs) cs))
-- (T parent (dropWhile (\ childSubtree -> depth childSubtree == 2) $ filter (\ childSubtree -> isBranch childSubtree) children))

-- prune :: NTree a -> NTree a
-- prune (T parent children) 
--  | any (\ child -> isBranch child) children = (T parent (dropWhile (\ child -> depth child == 2) children))
--  | otherwise = (T parent children)

-- --prune :: NTree a -> NTree a
-- prune (T parent children) 
--  | depth children == 1 = (T parent [])
--  | 
 
 --------------------------------------------------------------------------------------------------------------------

-- main :: IO ()
-- main = do
--     print $ length [T 1 [T 3 [T 5 [], T 6 []]]]
--     print $ depth t
--     print $ isBranch (T 1 [T 3 [T 5 []]])
--     print $ prune t -- == T 1 [T 2 [T 3 []], T 4 [T 5 []], T 7 [T 8 [], T 9 [T 10 []]]]

-- data NTree a = T a [(NTree a)]
--     deriving (Eq, Show, Foldable)

-- t :: Num a => NTree a
-- t = T 1 [T 2 [T 3 []], T 4 [T 5 [T 6 []]], T 7 [T 8 [], T 9 [T 10 [T 11 []]]]]

-- depth :: NTree a -> Int
-- depth (T parent []) = 1 
-- depth (T parent children) = 1 + (sum $ map depth children)

-- isBranch :: NTree a -> Bool   --да има само 1 дете 
-- isBranch (T parent []) = True
-- isBranch (T parent children) = length children == 1 && (all (\ child -> (isBranch child)) children) 

-- prune :: NTree a -> NTree a
-- prune (T parent children)
--     | all isBranch children = (T parent (dropWhile (\ c -> length (getChildren c) == 2 && (depth (T parent children) > 2)) children))
--     | otherwise = T parent (map prune children)
--   where
--     getChildren (T _ c) = c



